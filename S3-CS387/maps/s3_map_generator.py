#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
S3 Procedural Map Generator — clustered biomes via Voronoi (Worley) + Poisson-disk seeds.
Defaults: --width 72 --height 32 --seed 424242
Footprints: WPeasant 1x1, WTownhall 4x4, WGoldMine 3x3
Encoding: '.' (ground), 'w' (water), 't' (forest)
"""

import argparse, math, random, xml.etree.ElementTree as ET
from collections import deque

# --- Tiles
G='.'; W='w'; T='t'

# --- Footprints and land margin (all spawned entities must sit on ground with this buffer)
FOOTPRINT={"WPeasant":(1,1),"WTownhall":(4,4),"WGoldMine":(3,3)}
MARGIN=1

FOUR=[(-1,0),(1,0),(0,-1),(0,1)]
MOORE=[(-1,-1),(0,-1),(1,-1),(-1,0),(1,0),(-1,1),(0,1),(1,1)]

class Grid:
    def __init__(self,w,h,fill=G):
        self.w,self.h=w,h
        self.a=[[fill for _ in range(h)] for _ in range(w)]
    def inb(self,x,y): return 0<=x<self.w and 0<=y<self.h
    def get(self,x,y): return self.a[x][y]
    def set(self,x,y,v): self.a[x][y]=v
    def rows_as_strings(self):
        return [''.join(self.a[x][y] for x in range(self.w)) for y in range(self.h)]

# -------------------- Connectivity, smoothing, corridors --------------------

def flood_fill_ground(g, start):
    Q=deque([start]); seen={start}
    while Q:
        x,y=Q.popleft()
        for dx,dy in FOUR:
            xx,yy=x+dx,y+dy
            if g.inb(xx,yy) and g.get(xx,yy)==G and (xx,yy) not in seen:
                seen.add((xx,yy)); Q.append((xx,yy))
    return seen

def all_ground_components(g):
    seen=set(); comps=[]
    for y in range(g.h):
        for x in range(g.w):
            if g.get(x,y)==G and (x,y) not in seen:
                comp=flood_fill_ground(g,(x,y)); comps.append(comp); seen|=comp
    return comps

def connect_all_ground(g):
    comps=all_ground_components(g)
    if not comps: return
    comps.sort(key=len, reverse=True)
    main=comps[0]
    for comp in comps[1:]:
        best=None; bestd=10**9
        for (x1,y1) in main:
            for (x2,y2) in comp:
                d=abs(x1-x2)+abs(y1-y2)
                if d<bestd: bestd=d; best=((x1,y1),(x2,y2))
            if bestd<=1: break
        carve_line_to_ground(g, best[0], best[1])
        main=flood_fill_ground(g, next(iter(main)))

def carve_line_to_ground(g, a, b):
    x,y=a; tx,ty=b
    def clear(xx,yy):
        if g.inb(xx,yy): g.set(xx,yy,G)
    while x!=tx:
        x += 1 if tx>x else -1
        clear(x,y); clear(x,y+1); clear(x,y-1)
    while y!=ty:
        y += 1 if ty>y else -1
        clear(x,y); clear(x+1,y); clear(x-1,y)

def majority_filter_once(g):
    newa=[col[:] for col in g.a]
    for x in range(g.w):
        for y in range(g.h):
            counts={G:0,W:0,T:0}
            for dx,dy in MOORE+[(0,0)]:
                xx,yy=x+dx,y+dy
                if g.inb(xx,yy): counts[g.get(xx,yy)]+=1
            newa[x][y]=max(counts, key=counts.get)
    g.a=newa

# -------------------- Poisson disk seeds + Voronoi (Worley) --------------------

def bridson_poisson_disk(width, height, r, rng, left_half=True):
    """
    Bridson's Poisson-disk sampling in 2D (O(N)) with min distance r.
    If left_half=True, domain is [0, width//2) × [0, height); we mirror later.
    """
    w = width//2 if left_half else width
    cell_size = r / math.sqrt(2)
    gw, gh = int(math.ceil(w/cell_size)), int(math.ceil(height/cell_size))
    grid=[[-1]*gh for _ in range(gw)]
    def grid_coords(p):
        x,y=p; return int(x/cell_size), int(y/cell_size)
    def in_domain(p):
        x,y=p; return 0<=x<w and 0<=y<height
    def far_enough(p):
        gx,gy=grid_coords(p)
        for i in range(max(0,gx-2), min(gw-1,gx+2)+1):
            for j in range(max(0,gy-2), min(gh-1,gy+2)+1):
                qi=grid[i][j]
                if qi!=-1:
                    q=pts[qi]
                    if (p[0]-q[0])**2 + (p[1]-q[1])**2 < r*r:
                        return False
        return True

    k=30
    pts=[]; active=[]
    # init
    p0=(rng.uniform(0,w), rng.uniform(0,height))
    pts.append(p0); active.append(0)
    gx,gy=grid_coords(p0); grid[gx][gy]=0

    while active:
        idx = rng.choice(active)
        found=False
        for _ in range(k):
            ang=rng.uniform(0, 2*math.pi)
            rad=rng.uniform(r, 2*r)
            cand=(pts[idx][0]+rad*math.cos(ang), pts[idx][1]+rad*math.sin(ang))
            if in_domain(cand) and far_enough(cand):
                pts.append(cand)
                gx,gy=grid_coords(cand); grid[gx][gy]=len(pts)-1
                active.append(len(pts)-1)
                found=True
                break
        if not found:
            active.remove(idx)
    # mirror to full width
    seeds=[]
    for x,y in pts:
        seeds.append((x,y))
        mx= (width-1) - x  # mirror across vertical axis
        seeds.append((mx,y))
    return seeds

def assign_biomes_voronoi(g, seeds, labels):
    """
    Assign each tile to the nearest seed; label by labels[index].
    'labels' length == len(seeds). Use Euclidean distance.
    """
    for y in range(g.h):
        for x in range(g.w):
            best=None; bestd=10**9
            for i,(sx,sy) in enumerate(seeds):
                d=(x-sx)*(x-sx)+(y-sy)*(y-sy)
                if d<bestd:
                    bestd=d; best=i
            g.set(x,y, labels[best])

def relabel_to_match_targets(g, seeds, seed_labels, targets, rng):
    """
    Compute area per seed (Voronoi region size). If ratios deviate,
    relabel whole seeds (and their mirrors — because we constructed mirrored seed pairs)
    until we approach targets. This preserves clusteriness.
    """
    # region areas
    area=[0]*len(seeds)
    for y in range(g.h):
        for x in range(g.w):
            # find responsible seed again (small map -> simple search)
            best=None; bestd=10**9
            for i,(sx,sy) in enumerate(seeds):
                d=(x-sx)*(x-sx)+(y-sy)*(y-sy)
                if d<bestd: bestd=d; best=i
            area[best]+=1

    def totals():
        cw=ct=cg=0
        for i,a in enumerate(area):
            if seed_labels[i]==W: cw+=a
            elif seed_labels[i]==T: ct+=a
            else: cg+=a
        return cw,ct,cg

    total=g.w*g.h
    Wt,Tt,Gt=targets[W],targets[T],targets[G]
    def off():
        cw,ct,cg=totals()
        return (cw-Wt, ct-Tt, cg-Gt)

    # Pair seeds by mirror (we added them in mirrored pairs)
    pairs=[(2*i,2*i+1) for i in range(len(seeds)//2)]

    # Try to move whole pairs between classes to reduce deviation
    for _ in range(200):
        dw,dt,dg=off()
        if abs(dw)+abs(dt)+abs(dg) <= 0.02*total: break
        # pick a direction to fix
        if dw<0:
            # need more water: find a non-water pair with the largest area and turn it to water
            cand=max((p for p in pairs if seed_labels[p[0]]!=W), key=lambda p: area[p[0]]+area[p[1]], default=None)
            if cand:
                for idx in cand: seed_labels[idx]=W
                continue
        if dt<0:
            cand=max((p for p in pairs if seed_labels[p[0]]!=T), key=lambda p: area[p[0]]+area[p[1]], default=None)
            if cand:
                for idx in cand: seed_labels[idx]=T
                continue
        if dw>0:
            # too much water: shrink water by converting smallest water pair to ground
            cand=min((p for p in pairs if seed_labels[p[0]]==W), key=lambda p: area[p[0]]+area[p[1]], default=None)
            if cand:
                for idx in cand: seed_labels[idx]=G
                continue
        if dt>0:
            cand=min((p for p in pairs if seed_labels[p[0]]==T), key=lambda p: area[p[0]]+area[p[1]], default=None)
            if cand:
                for idx in cand: seed_labels[idx]=G
                continue
        break  # nothing helped
    # reassign tiles using updated labels
    assign_biomes_voronoi(g, seeds, seed_labels)

# -------------------- Placement helpers --------------------

def rect_on_ground(g, x0, y0, w, h, margin):
    xm0, ym0 = x0 - margin, y0 - margin
    xm1, ym1 = x0 + w - 1 + margin, y0 + h - 1 + margin
    if xm0 < 0 or ym0 < 0 or xm1 >= g.w or ym1 >= g.h: return False
    for x in range(xm0, xm1+1):
        for y in range(ym0, ym1+1):
            if g.get(x,y) != G: return False
    return True

def find_clear_rect_near(g, center, w, h, margin, max_radius=30):
    cx,cy=center; guess=(cx - w//2, cy - h//2)
    def candidates(r):
        for dx in range(-r,r+1):
            dy=r-abs(dx)
            for sgn in (-1,1):
                yield (guess[0]+dx, guess[1]+sgn*dy)
    if rect_on_ground(g, guess[0], guess[1], w, h, margin): return guess
    for r in range(1, max_radius+1):
        for (x0,y0) in candidates(r):
            if rect_on_ground(g, x0, y0, w, h, margin): return (x0,y0)
    return None

def clear_rect(g, tl, w, h):
    (x0,y0)=tl
    for x in range(x0-MARGIN, x0+w+MARGIN):
        for y in range(y0-MARGIN, y0+h+MARGIN):
            if g.inb(x,y): g.set(x,y,G)

# -------------------- Map generation --------------------

def generate(width,height,seed):
    rng=random.Random(seed)
    g=Grid(width,height,fill=G)

    # 1) Clustered biome layout: Poisson-disk seeds (left half), mirror, Voronoi assign
    # Choose min spacing so we get a handful of sizable regions.
    min_r = max(5, min(width,height)//6)
    seeds = bridson_poisson_disk(width,height,min_r,rng,left_half=True)

    # Assign labels to seed PAIRS to roughly meet targets
    total=width*height
    target_W = int(round(total*0.10))
    target_T = int(round(total*0.30))
    target_G = total - target_W - target_T
    targets={W:target_W,T:target_T,G:target_G}

    # Initial labels: assign per pair so left/right match
    pairs=[(2*i,2*i+1) for i in range(len(seeds)//2)]
    rng.shuffle(pairs)
    labels=[G]*len(seeds)
    # Roughly allocate by area proxy: equal per pair
    n_pairs=len(pairs)
    w_pairs=max(1, int(round(0.10*n_pairs)))
    t_pairs=max(1, int(round(0.30*n_pairs)))
    for i,p in enumerate(pairs):
        lab = W if i < w_pairs else (T if i < w_pairs+t_pairs else G)
        for idx in p: labels[idx]=lab

    assign_biomes_voronoi(g, seeds, labels)
    # Slight cleanups to remove 1-tile tendrils without destroying clusters
    majority_filter_once(g)

    # 2) Ensure connected land and carve a mid-path between bases
    # (choose base rows on middle band where not water)
    def first_nonwater_x(y, left=True):
        xs=range(3,width//3) if left else range(width-4,2*width//3,-1)
        for x in xs:
            if g.get(x,y)!=W: return x
        return 3 if left else width-4
    y_mid=rng.randint(height//3, 2*height//3)
    p1=(first_nonwater_x(y_mid,True), y_mid)
    p2=(first_nonwater_x(y_mid,False), y_mid)
    carve_line_to_ground(g,p1,p2)
    connect_all_ground(g)

    # 3) Ratio correction by relabeling whole Voronoi regions (keeps clusters)
    relabel_to_match_targets(g, seeds, labels, targets, rng)
    majority_filter_once(g)
    connect_all_ground(g)

    # 4) Place entities with footprint + margin on ground
    tw,th=FOOTPRINT["WTownhall"]; pw,ph=FOOTPRINT["WPeasant"]; mw,mh=FOOTPRINT["WGoldMine"]
    t1 = find_clear_rect_near(g,(p1[0],p1[1]-5),tw,th,MARGIN) or (p1[0]-tw//2,p1[1]-5-th//2)
    t2 = find_clear_rect_near(g,(p2[0],p2[1]-5),tw,th,MARGIN) or (p2[0]-tw//2,p2[1]-5-th//2)
    if not rect_on_ground(g,t1[0],t1[1],tw,th,MARGIN): clear_rect(g,t1,tw,th)
    if not rect_on_ground(g,t2[0],t2[1],tw,th,MARGIN): clear_rect(g,t2,tw,th)

    s1 = find_clear_rect_near(g,(p1[0]-3,p1[1]+5),pw,ph,MARGIN) or (p1[0]-3,p1[1]+5)
    s2 = find_clear_rect_near(g,(p2[0]+3,p2[1]+5),pw,ph,MARGIN) or (p2[0]+3,p2[1]+5)
    if not rect_on_ground(g,s1[0],s1[1],pw,ph,MARGIN): clear_rect(g,s1,pw,ph)
    if not rect_on_ground(g,s2[0],s2[1],pw,ph,MARGIN): clear_rect(g,s2,pw,ph)

    def place_mine(base, radius=12):
        bx,by=base
        for _ in range(200):
            r=rng.randint(max(6,radius-4), radius+4)
            th=rng.uniform(-0.9,0.9)
            cx=int(round(bx + r*math.cos(th)))
            cy=int(round(by + r*math.sin(th)))
            pos=find_clear_rect_near(g,(cx,cy),mw,mh,MARGIN)
            if pos: return pos
        return find_clear_rect_near(g,(bx+8,by),mw,mh,MARGIN) or (bx+8,by)

    m1=place_mine(p1)
    m2_guess=((width-1)-(m1[0]+mw-1)-(mw-1), m1[1])
    m2=find_clear_rect_near(g,m2_guess,mw,mh,MARGIN) or m2_guess
    if not rect_on_ground(g,m1[0],m1[1],mw,mh,MARGIN): clear_rect(g,m1,mw,mh)
    if not rect_on_ground(g,m2[0],m2[1],mw,mh,MARGIN): clear_rect(g,m2,mw,mh)

    # 5) Final safety: connect land and ensure unit-to-unit reachability
    connect_all_ground(g)
    def center(tl,sz): (x0,y0)=tl; (w,h)=sz; return (x0+w//2,y0+h//2)
    cts=[center(t1,(tw,th)), center(t2,(tw,th)), center(s1,(pw,ph)), center(s2,(pw,ph))]
    seen=flood_fill_ground(g, cts[0])
    for pt in cts[1:]:
        if pt not in seen: carve_line_to_ground(g, cts[0], pt)
    connect_all_ground(g)

    return g, s1, t1, s2, t2, m1, m2

# -------------------- XML export with strict IDs --------------------

def export_s3_xml(path, grid, peas1, town1, peas2, town2, m1, m2):
    root=ET.Element("game")

    def write_entity(eid, etype, items):
        e=ET.SubElement(root,"entity",{"id":str(eid)})
        t=ET.SubElement(e,"type"); t.text=etype
        for k,v in items: ET.SubElement(e,k).text=str(v)

    # Map entity id=0 with width/height and <background>
    write_entity(0,"map",[("width",grid.w),("height",grid.h)])
    map_entity=root.find("./entity[@id='0']")
    bg=ET.SubElement(map_entity,"background")
    for row in grid.rows_as_strings():
        ET.SubElement(bg,"row").text=row  # NO SPACES

    # Players
    write_entity(14,"WPlayer",[("gold",2000),("wood",1500),("owner","player1")])
    write_entity(15,"WPlayer",[("gold",2000),("wood",1500),("owner","player2")])

    # Mines (two total; no owner)
    (mx1,my1)=m1; (mx2,my2)=m2
    write_entity(4,"WGoldMine",[("x",mx1),("y",my1),("remaining_gold",100000),("current_hitpoints",25500)])
    write_entity(5,"WGoldMine",[("x",mx2),("y",my2),("remaining_gold",100000),("current_hitpoints",25500)])

    # Units
    (sx1,sy1)=peas1; (tx1,ty1)=town1
    (sx2,sy2)=peas2; (tx2,ty2)=town2
    write_entity(2,"WPeasant",[("x",sx1),("y",sy1),("owner","player1"),("current_hitpoints",30)])
    write_entity(3,"WTownhall",[("x",tx1),("y",ty1),("owner","player1"),("current_hitpoints",2400)])
    write_entity(7,"WPeasant",[("x",sx2),("y",sy2),("owner","player2"),("current_hitpoints",30)])
    write_entity(8,"WTownhall",[("x",tx2),("y",ty2),("owner","player2"),("current_hitpoints",2400)])

    ET.ElementTree(root).write(path, encoding="utf-8", xml_declaration=True)

# -------------------- Main --------------------

def main():
    ap=argparse.ArgumentParser()
    ap.add_argument("--width", type=int, default=72)
    ap.add_argument("--height", type=int, default=32)
    ap.add_argument("--seed", type=int, default=424242)
    ap.add_argument("--out", type=str, default="map.xml")
    args=ap.parse_args()

    grid, s1, t1, s2, t2, m1, m2 = generate(args.width,args.height,args.seed)
    export_s3_xml(args.out, grid, s1, t1, s2, t2, m1, m2)

if __name__=="__main__":
    main()
