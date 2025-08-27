
package s3.ai.builtin;

import java.io.IOException;
import java.util.LinkedList;
import java.util.List;

import s3.ai.AI;
import s3.base.S3;
import s3.base.S3Action;
import s3.entities.*;
import s3.util.Pair;

import s3.ai.builtin.files_uc49.*;

import java.util.Map;
import java.util.HashMap;
import s3.entities.WUnit;

public class utsav_uc49 implements AI {

    private static final int MIN_MINE_SLOTS = 5;

    private static int mapW(S3 game) { return game.getMap().getWidth(); }
    private static int mapH(S3 game) { return game.getMap().getHeight(); }
    private static boolean inBounds(S3 g, int x, int y) {
        return x >= 0 && y >= 0 && x < mapW(g) && y < mapH(g);
    }
    private static int manhattan(int ax, int ay, int bx, int by) {
        return Math.abs(ax - bx) + Math.abs(ay - by);
    }
    private static final int[][] DIR4 = {{1,0},{-1,0},{0,1},{0,-1}};

    private static int distToRect(int px, int py, int rx, int ry, int rw, int rh) {
        int dx = (px < rx) ? (rx - px) : (px > rx + rw - 1 ? px - (rx + rw - 1) : 0);
        int dy = (py < ry) ? (ry - py) : (py > ry + rh - 1 ? py - (ry + rh - 1) : 0);
        return dx + dy;
    }

    // Helper to treat moving units as passable in reachability (buildings still block).
    private static boolean isDynamicUnit(S3Entity e) {
        return e instanceof WPeasant
                || e instanceof WFootman
                || e instanceof WArcher
                || e instanceof WKnight
                || e instanceof WCatapult;
    }

    /** Collect top-K trees by (distance to mine ASC, grassNeighbors DESC). */
    private static java.util.List<s3.util.Pair<Integer,Integer>>
    topTreesNearMine(S3 game, WGoldMine m, int k) {
        final int mx = m.getX(), my = m.getY();
        final int mw = m.getWidth(), mh = m.getLength();

        int pad = 14;
        int xmin = Math.max(0, mx - pad), xmax = Math.min(mapW(game)-1, mx + mw + pad);
        int ymin = Math.max(0, my - pad), ymax = Math.min(mapH(game)-1, my + mh + pad);

        class Cand { int x,y, d, gN; }
        java.util.ArrayList<Cand> all = new java.util.ArrayList<>();

        for (int y = ymin; y <= ymax; y++) {
            for (int x = xmin; x <= xmax; x++) {
                if (!isTree(game, x, y)) continue;
                Cand c = new Cand();
                c.x = x; c.y = y;
                c.d = distToRect(x, y, mx, my, mw, mh);
                c.gN = grassNeighborCount(game, x, y);
                all.add(c);
            }
        }

        all.sort((a,b) -> {
            if (a.d != b.d) return Integer.compare(a.d, b.d); // nearer to mine first
            return -Integer.compare(a.gN, b.gN);               // more WOGrass neighbors first
        });

        java.util.ArrayList<s3.util.Pair<Integer,Integer>> out = new java.util.ArrayList<>();
        for (int i = 0; i < all.size() && i < k; i++) {
            out.add(new s3.util.Pair<>(all.get(i).x, all.get(i).y));
        }
        return out;
    }

    /** Action: send available peasants to chop the best trees (as defined above). */
    private BTNode.Status act_ClearMineAccess_ByNearestLeastNeighbors(
            S3 game, WPlayer player, java.util.List<S3Action> actions) {

        WGoldMine m = nearestMine(game, player);
        if (m == null) return BTNode.Status.FAILURE;

        // Get available peasants (relaxed idleness check).
        java.util.ArrayList<WPeasant> avail = new java.util.ArrayList<>();
        for (WUnit u : game.getUnitTypes(player, s3.entities.WPeasant.class)) {
            S3Action st = u.getStatus();
            boolean free = (st == null) ||
                    (st.m_action == S3Action.ACTION_STAND_GROUND) ||
                    (st.m_action == S3Action.ACTION_NONE) ||
                    (st.m_action == S3Action.ACTION_MOVE);
            if (free) avail.add((WPeasant) u);
        }
        if (avail.isEmpty()) return BTNode.Status.RUNNING;

        // Pick that many top trees.
        var picks = topTreesNearMine(game, m, avail.size());
        if (picks.isEmpty()) return BTNode.Status.FAILURE; // no trees in the scan region (unlikely)

        // Greedy one-to-one assignment in sorted order.
        int assigned = 0;
        for (int i = 0; i < avail.size() && i < picks.size(); i++) {
            var t = picks.get(i);
            actions.add(new S3Action(avail.get(i).entityID, S3Action.ACTION_HARVEST, t.m_a, t.m_b));
            assigned++;
        }

        return (assigned > 0) ? BTNode.Status.SUCCESS : BTNode.Status.RUNNING;
    }


    private static boolean isGrass(S3 game, int x, int y) {
        S3Entity e = game.getEntity(x, y);
        return (e instanceof WOGrass);
    }

    private static boolean isSolid(S3 game, int x, int y) {
        // NB: must depend on the *dual-source* grass/tree tests, not entity-only.
        return !isGrass(game, x, y) && !isTree(game, x, y);
    }

    private static int grassNeighborCount(S3 game, int x, int y) {
        int c = 0;
        for (int[] d : DIR4) {
            int nx = x + d[0], ny = y + d[1];
            if (inBounds(game, nx, ny) && isGrass(game, nx, ny)) c++;
        }
        return c;
    }


    public static final int    TOWNHALL_MINE_DISTANCE_X       = 1;
    public static final int    TOWNHALL_MINE_DISTANCE_Y       = 1;

    String m_playerID;

    private final BTNode root;

    private boolean stage2;

    private static final Map<Class<? extends WUnit>, Pair<Integer,Integer>> COST_CACHE = new HashMap<>();

    private static <T extends WUnit> Pair<Integer,Integer> getCost(Class<T> clazz) {
        return COST_CACHE.computeIfAbsent(clazz, k -> {
            try {
                T u = (T) k.getDeclaredConstructor().newInstance();
                return new Pair<>(u.getCost_gold(), u.getCost_wood());
            } catch (ReflectiveOperationException e) {
                throw new IllegalStateException("Cannot instantiate " + k.getName(), e);
            }
        });
    }

    private static boolean isWalkable(S3 game, int x, int y) {
        // Matches your A* collision policy: ground tiles are WOGrass.
        S3Entity e = game.getEntity(x, y);
        return (e instanceof WOGrass);
    }

    private static boolean isTree(S3 game, int x, int y) {
        // If your build uses s3.entities.WOTree for trees, this will work.
        // (If you’re on a tileset that encodes 't' in background instead, swap this check accordingly.)
        S3Entity e = game.getEntity(x, y);
        return (e instanceof s3.entities.WOTree);
    }

    private static <T extends WUnit> boolean canAfford(WPlayer p, Class<T> clazz, int count) {
        Pair<Integer,Integer> c = getCost(clazz);
        int totalGold = c.m_a * count;
        int totalWood = c.m_b * count;
        return p.getGold() >= totalGold && p.getWood() >= totalWood;
    }


    public utsav_uc49(String playerID) {
        m_playerID = playerID;
        this.root       = BTTreeLoader.load(this);
    }

    public void gameEnd() {
        stillTicksById.clear();
        lastPosById.clear();
    }

    public void gameStarts() {
        stage2 = false;
    }

    public boolean evaluateConditionStatic(String cond, S3 game, WPlayer player) {

        switch (cond) {
            case "NoBase":
                return game.getUnitType(player, WTownhall.class) == null;
            case "NoBarracks":
                return game.getUnitType(player, WBarracks.class) == null;
            case "BaseExistsAndPeasantCountLessThan4":
                return game.getUnitType(player, WTownhall.class) != null
                        && game.getUnitTypes(player, WPeasant.class).size() < 4;
            case "BaseExistsAndPeasantCountLessThan10":
                return game.getUnitType(player, WTownhall.class) != null
                        && game.getUnitTypes(player, WPeasant.class).size() < 10;
            case "BarracksExistsAndIdleAndResources":
                List<WUnit> b = game.getUnitTypes(player, WBarracks.class).stream().filter(u -> u.getStatus() == null).toList();
                for(WUnit u : b) {
                    if (u != null && u.getStatus() == null && canAfford(player, WFootman.class, 1)){
                        return true;
                    }
                }
                return false;
            case "EnemyAttackingAndIdleTroop":
                // Check if any enemy unit is currently issuing an ATTACK action
                // targeting one of our units.
                for (WUnit enemy : game.getUnits()) {
                    if (!player.getOwner().equals(enemy.getOwner())) {
                        S3Action status = enemy.getStatus();
                        if (status != null && status.m_action == S3Action.ACTION_ATTACK) {
                            // The first parameter is the target entityID
                            int targetId = (Integer) status.m_parameters.get(0);
                            WUnit target = game.getUnit(targetId);
                            if (target != null && player.getOwner().equals(target.getOwner())) {
                                // We're under attack and we have at least one idle footmam
                                List<WUnit> troops = game.getUnitTypes(player, s3.entities.WFootman.class);
                                troops.addAll(game.getUnitTypes(player, s3.entities.WArcher.class));
                                troops.addAll(game.getUnitTypes(player, s3.entities.WKnight.class));
                                troops.addAll(game.getUnitTypes(player, s3.entities.WCatapult.class));
                                return !troops
                                        .stream()
                                        .filter(u -> u.getStatus() == null)
                                        .toList().isEmpty();
                            }
                        }
                    }
                }
                return false;
            case "EnemyAttacking":
                for (WUnit enemy : game.getUnits()) {
                    if (!player.getOwner().equals(enemy.getOwner())) {
                        S3Action status = enemy.getStatus();
                        if (status != null && status.m_action == S3Action.ACTION_ATTACK) {
                            return true;
                        }
                    }
                }
                return false;
            case "IdleTroopCountAtLeast6":
                List<WUnit> troops = game.getUnitTypes(player, s3.entities.WFootman.class);
                troops.addAll(game.getUnitTypes(player, s3.entities.WArcher.class));
                troops.addAll(game.getUnitTypes(player, s3.entities.WKnight.class));
                troops.addAll(game.getUnitTypes(player, s3.entities.WCatapult.class));
                return troops
                        .stream().filter(x->x.getStatus()==null).count() >= 6;
            case "IdlePeasants":
                return checkIdlePeasants(game);
            case "NoPeasantsInWood":
                return checkPeasantsInWood(game);
            case "LessThan6Footmen":
                return game.getUnitTypes(player, s3.entities.WFootman.class).size() < 6;
            case "LessThan4Archers":
                return game.getUnitTypes(player, s3.entities.WArcher.class).size() < 4;
            case "LessThan3Knights":
                boolean state = game.getUnitTypes(player, s3.entities.WKnight.class).size() < 3;
                if (state) {
                    stage2 = true;
                }
                return state;
            case "LessThan6Knights":
                return game.getUnitTypes(player, s3.entities.WKnight.class).size() < 6;
            case "LessThan10Knights":
                return game.getUnitTypes(player, s3.entities.WKnight.class).size() < 10;
            case "LessThan2Barracks":
                return game.getUnitTypes(player, s3.entities.WBarracks.class).size() < 2;
            case "Stage2":
                return stage2;
            case "NoTowers":
                return game.getUnitType(player, WTower.class) == null;
            case "CanAffordTowers":
                return canAfford(player, WTower.class, 1);
            case "LotsOfResources":
                return canAfford(player, WFortress.class, 1);
            case "LessThan3Towers":
                return game.getUnitTypes(player, WTower.class).size() < 3;
            case "ReadyForBigAttack":
                return (game.getUnitTypes(player, WFootman.class).size() >= 7 && game.getUnitTypes(player, WKnight.class).size() >= 2) ||
                        (game.getUnitTypes(player, WFootman.class).size() >= 6 && game.getUnitTypes(player, WKnight.class).size() >= 4) ||
                        game.getUnitTypes(player, WKnight.class).size() >= 6 ||
                        game.getUnitTypes(player, WFootman.class).size() >= 12;
            case "LotsOfIdleTroop":
                int totalTroops = game.getUnitTypes(player, WFootman.class).size() +
                        game.getUnitTypes(player, WArcher.class).size() +
                        game.getUnitTypes(player, WKnight.class).size() +
                        game.getUnitTypes(player, WCatapult.class).size();
                return totalTroops > 5;
            case "NearestMineBlocked":
                return cond_NearestMineBlocked(game, player);
            default:
                return false;
        }
    }

    public BTNode.Status performAction(
            String act, S3 game, WPlayer player, List<S3Action> actions) {

        switch (act) {
            case "BuildBase": {
                return checkTownhall(game, player, actions);
            }
            case "BuildBarracks": {
                return checkBarracks(game, player, actions, false);
            }
            case "TrainPeasant", "SendPeasantsToWork": {
                return checkPeasants(game, player, actions, 3, 1, false);
            }
            case "TrainFootmen": {
                return buildFootmen(game, player, actions);
            }
            case "Defend": {
                // Attack nearest enemy unit
                WUnit target = game.getUnits().stream()
                        .filter(u-> (!player.getOwner().equals(u.getOwner())) && (u.getStatus() != null) &&
                                (u.getStatus().m_action == S3Action.ACTION_ATTACK) &&
                                ((u.getClass() == WCatapult.class) ||
                                        (u.getClass() == WArcher.class) ||
                                        (u.getClass() == WKnight.class) ||
                                        (u.getClass() == WFootman.class)))
                        .findAny().orElse(null);
                if (target!=null) {
                    List<WUnit> troops = game.getUnitTypes(player, s3.entities.WFootman.class);
                    troops.addAll(game.getUnitTypes(player, s3.entities.WArcher.class));
                    troops.addAll(game.getUnitTypes(player, s3.entities.WKnight.class));
                    troops.addAll(game.getUnitTypes(player, s3.entities.WCatapult.class));

                    if (troops.isEmpty()) {
                        return BTNode.Status.FAILURE;
                    }

                    troops.stream().filter(x->x.getStatus()==null)
                            .forEach(x->actions.add(
                                    new S3Action(x.entityID, S3Action.ACTION_ATTACK, target.entityID)));
                    return BTNode.Status.SUCCESS;
                }
                return BTNode.Status.FAILURE;
            }
            case "AttackEnemy": {
                return attack(game, player, actions);
            }
            case "MoveTroop": {
                return moveTroop(game, player, actions);
            }
            case "SendPeasantsToWood": {
                if (stage2) {
                    return checkPeasants(game, player, actions, 2, 2, true);
                }
                return checkPeasants(game, player, actions, 2, 1, true);
            }
            case "TrainArcher" : {
                return buildArcher(game, player, actions);
            }
            case "TrainPeasant2": {
                return checkPeasants(game, player, actions, 8, 2, false);
            }
            case "TrainKnight" : {
                return buildKnight(game, player, actions);
            }
            case "BuildSecondBarracks" : {
                return checkBarracks(game, player, actions, true);
            }
            case "BuildTower" : {
                return checkTowers(game, player, actions, 1);
            }
            case "Build3Towers" : {
                return checkTowers(game, player, actions, 3);
            }
            case "ClearMineAccess": {

                if (!cond_NearestMineBlocked(game, player)) {
                    return BTNode.Status.SUCCESS;
                }

                return act_ClearMineAccess_ByNearestLeastNeighbors(game, player, actions);
            }
            default:
                return BTNode.Status.FAILURE;
        }
    }

    public void game_cycle(S3 game, WPlayer player, List<S3Action> actions)
            throws ClassNotFoundException, IOException {
        root.tick(game, player, actions);
    }

    private BTNode.Status attack(S3 game, WPlayer player, List<S3Action> actions) {
        List<WUnit> troops = game.getUnitTypes(player, s3.entities.WFootman.class);
        troops.addAll(game.getUnitTypes(player, s3.entities.WArcher.class));
        troops.addAll(game.getUnitTypes(player, s3.entities.WKnight.class));
        troops.addAll(game.getUnitTypes(player, s3.entities.WCatapult.class));

        if (troops.isEmpty()) {
            return BTNode.Status.FAILURE;
        }

        WPlayer enemy = null;
        for (WPlayer entity : game.getPlayers()) {
            if (entity != player) {
                enemy = entity;
                break;
            }
        }
        WUnit enemyTroop = game.getUnitType(enemy, WFortress.class);
        if (null == enemyTroop) {
            enemyTroop = game.getUnitType(enemy, WTower.class);
        }
        if (null == enemyTroop) {
            enemyTroop = game.getUnitType(enemy, WFootman.class);
        }
        if (null == enemyTroop) {
            enemyTroop = game.getUnitType(enemy, WKnight.class);
        }
        if (null == enemyTroop) {
            enemyTroop = game.getUnitType(enemy, WArcher.class);
        }
        if (null == enemyTroop) {
            enemyTroop = game.getUnitType(enemy, WCatapult.class);
        }
        if (null == enemyTroop) {
            enemyTroop = game.getUnitType(enemy, WTownhall.class);
        }
        if (null == enemyTroop) {
            enemyTroop = game.getUnitType(enemy, WPeasant.class);
        }
        if (null == enemyTroop) {
            enemyTroop = game.getUnitType(enemy, WBarracks.class);
        }
        if (null == enemyTroop) {
            enemyTroop = game.getUnitType(enemy, WLumberMill.class);
        }
        if (null == enemyTroop) {
            enemyTroop = game.getUnitType(enemy, WBlacksmith.class);
        }
        if (null == enemyTroop) {
            enemyTroop = game.getUnitType(enemy, WFortress.class);
        }
        if (null == enemyTroop) {
            enemyTroop = game.getUnitType(enemy, WStable.class);
        }
        if (null == enemyTroop) {
            return BTNode.Status.FAILURE;
        }

        boolean attacked = false;

        for(WUnit u:troops) {
            actions.add(new S3Action(u.entityID,S3Action.ACTION_ATTACK, enemyTroop.entityID));
            attacked = true;
        }

        return attacked ? BTNode.Status.SUCCESS : BTNode.Status.FAILURE;

    }

    private BTNode.Status moveTroop(S3 game, WPlayer player, List<S3Action> actions) {
        List<WUnit> troops = game.getUnitTypes(player, s3.entities.WFootman.class);
        troops.addAll(game.getUnitTypes(player, s3.entities.WArcher.class));
        troops.addAll(game.getUnitTypes(player, s3.entities.WKnight.class));
        troops.addAll(game.getUnitTypes(player, s3.entities.WCatapult.class));

        troops = troops.stream().filter(u -> u.getStatus() == null).toList();

        if (troops.isEmpty()) {
            return BTNode.Status.FAILURE;
        }

        WUnit landmark = game.getUnitType(player, WBarracks.class);

        if (landmark == null) {
            landmark = game.getUnitType(player, WTownhall.class);
        }

        if (landmark == null) {
            return BTNode.Status.FAILURE;
        }

        Pair<Integer, Integer> loc = game.findFreeSpace(landmark.getX(), landmark.getY(), 1);

        for(WUnit u:troops) {
            actions.add(new S3Action(u.entityID,S3Action.ACTION_MOVE, loc.m_a, loc.m_b));
        }

        return BTNode.Status.SUCCESS;

    }

    /**
     * trains a footman if there is enough gold.
     *
     * @param game
     * @param player
     * @param actions
     */
    private BTNode.Status buildFootmen(S3 game, WPlayer player, List<S3Action> actions) {

        for (WUnit b : game.getUnitTypes(player, WBarracks.class)) {
            if (b != null && b.getStatus() == null && canAfford(player, WFootman.class, 1)) {
                actions.add(new S3Action(b.entityID, S3Action.ACTION_TRAIN, WFootman.class.getSimpleName()));
                return BTNode.Status.SUCCESS;
            }
        }

        return BTNode.Status.FAILURE;
    }

    private BTNode.Status buildArcher(S3 game, WPlayer player, List<S3Action> actions) {

        for (WUnit b : game.getUnitTypes(player, WBarracks.class)) {
            if (b != null && b.getStatus() == null && canAfford(player, WArcher.class, 1)) {
                actions.add(new S3Action(b.entityID, S3Action.ACTION_TRAIN, WArcher.class.getSimpleName()));
                return BTNode.Status.SUCCESS;
            }
        }

        return BTNode.Status.FAILURE;
    }

    private BTNode.Status buildKnight(S3 game, WPlayer player, List<S3Action> actions) {

        for (WUnit b : game.getUnitTypes(player, WBarracks.class)) {
            if (b != null && b.getStatus() == null && canAfford(player, WKnight.class, 1)) {
                actions.add(new S3Action(b.entityID, S3Action.ACTION_TRAIN, WKnight.class.getSimpleName()));
                return BTNode.Status.SUCCESS;
            }
        }

        return BTNode.Status.FAILURE;
    }

    // Tune to your tick rate. 10–16 works well at 20–30 FPS.
    private static final int STUCK_TICKS = 12;

    // Motion trackers
    private final Map<Integer, Integer> stillTicksById = new HashMap<>();
    private final Map<Integer, Pair<Integer,Integer>> lastPosById = new HashMap<>();

    private static boolean isTrulyBusy(S3Action st) {
        if (st == null) return false;
        switch (st.m_action) {
            case S3Action.ACTION_HARVEST:
            case S3Action.ACTION_BUILD:
            case S3Action.ACTION_REPAIR:
            case S3Action.ACTION_TRAIN:
            case S3Action.ACTION_ATTACK:
                return true;
            default:
                return false;
        }
    }

    private boolean checkIdlePeasants(S3 game) {
        boolean anyIdle = false;

        for (S3Entity e : game.getAllUnits()) {
            if (!(e instanceof WPeasant)) continue;
            if (!e.getOwner().equals(m_playerID)) continue;

            WPeasant p = (WPeasant) e;
            S3Action st = p.getStatus();

            // 1) Immediately idle if no status or explicitly non-busy
            if (!isTrulyBusy(st)) {
                anyIdle = true;
                // still update trackers so they stay fresh
            }

            // 2) Update stuck counters
            Pair<Integer,Integer> prev = lastPosById.get(p.entityID);
            int x = p.getX(), y = p.getY();
            if (prev != null && prev.m_a == x && prev.m_b == y) {
                stillTicksById.put(p.entityID, stillTicksById.getOrDefault(p.entityID, 0) + 1);
            } else {
                stillTicksById.put(p.entityID, 0);
                lastPosById.put(p.entityID, new Pair<>(x, y));
            }

            // 3) If the peasant is “busy” but hasn’t moved for a while, treat as idle/stalled
            if (isTrulyBusy(st)) {
                int stuck = stillTicksById.getOrDefault(p.entityID, 0);
                if (stuck >= STUCK_TICKS) {
                    anyIdle = true;
                }
            }
        }

        return anyIdle;
    }


    private boolean checkPeasantsInWood(S3 game) {
        int gp = 0;
        int wp = 0;

        List<WPeasant> freePeasants = new LinkedList<>();
        for (S3Entity e : game.getAllUnits()) {
            if (e instanceof WPeasant && e.getOwner().equals(m_playerID)) {
                WPeasant peasant = (WPeasant) e;
                S3Action st = peasant.getStatus();

                // Maintain your harvest counters exactly as before
                if (st != null && st.m_action == S3Action.ACTION_HARVEST) {
                    if (st.m_parameters.size() == 1) gp++;
                    else wp++;
                } else {
                    // Key change: treat non-busy (including MOVE/NONE/STOP/STAND_GROUND) as idle/free
                    if (!isTrulyBusy(st)) {
                        freePeasants.add(peasant);
                    }
                }
            }
        }


        return wp == 0;
    }

    /**
     * Checks that there is at least one peasant mining gold
     *
     * @param game
     * @param player
     * @param actions
     */
    private BTNode.Status checkPeasants(S3 game, WPlayer player, List<S3Action> actions, int nGoldPeasants, int nWoodPEasants, boolean goToWood) {
        int gp = 0;
        int wp = 0;
        BTNode.Status woodStatus = BTNode.Status.FAILURE;

        List<WPeasant> freePeasants = new LinkedList<WPeasant>();
        for(S3Entity e:game.getAllUnits()) {
            if (e instanceof WPeasant && e.getOwner().equals(m_playerID)) {
                WPeasant peasant = (WPeasant)e;
                if (peasant.getStatus()!=null && peasant.getStatus().m_action==S3Action.ACTION_HARVEST) {
                    if (peasant.getStatus().m_parameters.size()==1) gp++;
                    else wp++;
                } else {
                    if (peasant.getStatus()==null)
                        freePeasants.add(peasant);
                }
            }
        }

        if (!goToWood) {
            if (gp < nGoldPeasants && !freePeasants.isEmpty()) {
                WPeasant peasant = freePeasants.get(0);
                List<WUnit> mines = game.getUnitTypes(null, WGoldMine.class);
                WGoldMine mine = null;
                int leastDist = 9999;
                for (WUnit unit : mines) {
                    int dist = Math.abs(unit.getX() - peasant.getX())
                            + Math.abs(unit.getY() - peasant.getY());
                    if (dist < leastDist) {
                        leastDist = dist;
                        mine = (WGoldMine) unit;
                    }
                }
                if (null != mine && !cond_NearestMineBlocked(game, player)) {
                    actions.add(new S3Action(peasant.entityID, S3Action.ACTION_HARVEST, mine.entityID));
                    freePeasants.remove(peasant);
                    if (freePeasants.isEmpty()) {
                        return BTNode.Status.RUNNING;
                    }
                }
            }
        }

        if (wp<nWoodPEasants && !freePeasants.isEmpty()) {
            WPeasant peasant = freePeasants.get(0);
            List<WOTree> trees = new LinkedList<WOTree>();
            for(int i = 0;i<game.getMap().getWidth();i++) {
                for(int j = 0;j<game.getMap().getHeight();j++) {
                    S3PhysicalEntity e = game.getMap().getEntity(i, j);
                    if (e instanceof WOTree) trees.add((WOTree)e);
                }
            }

            WOTree tree = null;
            int leastDist = 9999;
            WUnit townhall = game.getUnitType(player, WTownhall.class);
            for (WOTree unit : trees) {
                int dist = Math.abs(unit.getX() - townhall.getX())
                        + Math.abs(unit.getY() - townhall.getY());
                if (dist < leastDist) {
                    leastDist = dist;
                    tree = unit;
                }
            }
            if (null != tree && !cond_NearestMineBlocked(game, player)) {
                actions.add(new S3Action(peasant.entityID, S3Action.ACTION_HARVEST, tree.getX(),
                        tree.getY()));

                freePeasants.remove(peasant);

                woodStatus = BTNode.Status.SUCCESS;
            }
        }

        if ((gp<nGoldPeasants || wp<nWoodPEasants) && freePeasants.isEmpty()) {
            WTownhall th = (WTownhall) game.getUnitType(player, WTownhall.class);
            if (th!=null && th.getStatus()==null) {
                if (!canAfford(player, WPeasant.class, 1)) {
                    System.out.println("Can't afford to build peasant");
                    return BTNode.Status.FAILURE;
                }
                actions.add(new S3Action(th.entityID,S3Action.ACTION_TRAIN, WPeasant.class.getSimpleName()));
                return BTNode.Status.RUNNING;
            }
        }

        while (!freePeasants.isEmpty()) {
            WPeasant peasant = freePeasants.get(0);
            List<WUnit> mines = game.getUnitTypes(null, WGoldMine.class);
            WGoldMine mine = null;
            int leastDist = 9999;
            for (WUnit unit : mines) {
                int dist = Math.abs(unit.getX() - peasant.getX())
                        + Math.abs(unit.getY() - peasant.getY());
                if (dist < leastDist) {
                    leastDist = dist;
                    mine = (WGoldMine) unit;
                }
            }
            if (null != mine && !cond_NearestMineBlocked(game, player)) {
                actions.add(new S3Action(peasant.entityID, S3Action.ACTION_HARVEST, mine.entityID));
                freePeasants.remove(peasant);
            } else {
                break;
            }
        }

        if (woodStatus == BTNode.Status.FAILURE) {
            System.out.println("Cound't assign job to peasants");
        }

        return woodStatus;

    }

    /**
     * Checks that a barracks exists, and builds one if it doesn't
     *
     * @param game
     * @param player
     * @param actions
     */
    private BTNode.Status checkBarracks(S3 game, WPlayer player, List<S3Action> actions, boolean secondBarracks) {

        if (null == game.getUnitType(player, WBarracks.class) || secondBarracks) {
            List<WUnit> peasants = game.getUnitTypes(player, WPeasant.class);
            WPeasant peasant = null;
            for(WUnit p:peasants) {
                if (p.getStatus()!=null &&
                        p.getStatus().m_action==S3Action.ACTION_BUILD &&
                        p.getStatus().m_parameters.get(0).equals(WBarracks.class.getSimpleName())) {
                    // There is already a peasant building a barracks:
                    return BTNode.Status.RUNNING;
                }
            }
            for(WUnit p:peasants) {
                if (p.getStatus()==null ||
                        p.getStatus().m_action!=S3Action.ACTION_BUILD) {
                    peasant = (WPeasant)p;
                }
            }
            if (null == peasant) {
                checkPeasants(game, player, actions, 1, 1, false);
                return BTNode.Status.FAILURE;
            }

            WUnit landmark = game.getUnitType(player, WTownhall.class);

            if (landmark == null) {
                return BTNode.Status.FAILURE;
            }

            if (secondBarracks) {
                landmark = game.getUnitType(player, WBarracks.class);
            }

            int posX = landmark.getX() + landmark.getWidth() + 3;
            int posY = landmark.getY() + landmark.getHeight() + 3;

            if (posX >= game.getMap().getWidth()) {
                posX = landmark.getX() - landmark.getWidth() - 3;
            }

            if (posY >= game.getMap().getHeight()) {
                posY = landmark.getY() - landmark.getHeight() - 3;
            }

            // First try one locatino with space to walk around it:
            Pair<Integer, Integer> loc = game.findFreeSpace(posX, posY, 4);
            if (null == loc) {
                loc = game.findFreeSpace(posX, posY, 3);
                if (loc==null) return BTNode.Status.FAILURE;
            }

            if (!canAfford(player, WBarracks.class, 1)) {
                return BTNode.Status.FAILURE;
            }

            actions.add(new S3Action(peasant.entityID,S3Action.ACTION_BUILD, WBarracks.class.getSimpleName(), loc.m_a, loc.m_b));
        }
        return BTNode.Status.SUCCESS;
    }

    /**
     * Checks that a townhall exists, and builds one if it doesn't.
     *
     * @param game
     * @param player
     * @param actions
     */
    private BTNode.Status checkTownhall(S3 game, WPlayer player, List<S3Action> actions) {

        if (null == game.getUnitType(player, WTownhall.class)) {
            List<WUnit> peasants = game.getUnitTypes(player, WPeasant.class);
            WPeasant peasant = null;
            for(WUnit p:peasants) {
                if (p.getStatus()!=null &&
                        p.getStatus().m_action==S3Action.ACTION_BUILD &&
                        p.getStatus().m_parameters.get(0).equals(WTownhall.class.getSimpleName())) {
                    // There is already a peasant building a townhall:
                    return BTNode.Status.RUNNING;
                }
            }
            for(WUnit p:peasants) {
                if (p.getStatus()==null ||
                        p.getStatus().m_action!=S3Action.ACTION_BUILD) {
                    peasant = (WPeasant)p;
                }
            }
            if (null == peasant || !canAfford(player, WTownhall.class, 1)) {
                // we're screwed, can't build, can't harvest
                return BTNode.Status.FAILURE;
            }

            List<WUnit> mines = game.getUnitTypes(null, WGoldMine.class);
            WGoldMine mine = null;
            int leastDist = 9999;
            for (WUnit unit : mines) {
                int dist = Math.abs(unit.getX() - peasant.getX())
                        + Math.abs(unit.getY() - peasant.getY());
                if (dist < leastDist) {
                    leastDist = dist;
                    mine = (WGoldMine) unit;
                }
            }



            int posX = mine.getX() - TOWNHALL_MINE_DISTANCE_X;
            if (posX < 0) {
                posX = mine.getX() + TOWNHALL_MINE_DISTANCE_X;
            }
            int posY = mine.getY() - TOWNHALL_MINE_DISTANCE_Y;
            if (posY < 0) {
                posY = mine.getY() + TOWNHALL_MINE_DISTANCE_Y;
            }

            if (posX > mine.getX()) {
                posX += mine.getWidth();
            }

            if (posY > mine.getY()) {
                posY += mine.getHeight();
            }

            Pair<Integer, Integer> loc = game.findFreeSpace(posX, posY, 5);
            if (null == loc) {
                loc = game.findFreeSpace(posX, posY, 4);
                if (loc==null) return BTNode.Status.FAILURE;
            }
            actions.add(new S3Action(peasant.entityID,S3Action.ACTION_BUILD, WTownhall.class.getSimpleName(), loc.m_a, loc.m_b));
        }
        return BTNode.Status.SUCCESS;
    }

    boolean runningLastTime = false;

    private BTNode.Status checkTowers(S3 game, WPlayer player, List<S3Action> actions, int nTowers) {

        int nt = 0;
        for(S3Entity e:game.getAllUnits()) {
            if (e instanceof WTower && e.getOwner().equals(m_playerID)) nt++;
        }

        if (nt<nTowers) {
            List<WUnit> peasants = game.getUnitTypes(player, WPeasant.class);
            WPeasant peasant = null;
            for(WUnit p:peasants) {
                if (p.getStatus()!=null &&
                        p.getStatus().m_action==S3Action.ACTION_BUILD &&
                        p.getStatus().m_parameters.get(0).equals(WTower.class.getSimpleName())) {
                    // There is already a peasant building a barracks:
                    if (runningLastTime) {
                        runningLastTime = false;
                        return BTNode.Status.FAILURE;
                    }
                    runningLastTime = true;
                    return BTNode.Status.RUNNING;
                }
            }
            for(WUnit p:peasants) {
                if (p.getStatus()==null ||
                        p.getStatus().m_action!=S3Action.ACTION_BUILD) {
                    peasant = (WPeasant)p;
                }
            }
            if (null == peasant) return BTNode.Status.FAILURE;
            // First try one locatino with space to walk around it:
            Pair<Integer, Integer> loc = game.findFreeSpace(peasant.getX(), peasant.getY(), 4);
            if (null == loc) {
                loc = game.findFreeSpace(peasant.getX(), peasant.getY(), 2);
                if (loc==null) return BTNode.Status.FAILURE;
            }

            if (!canAfford(player, WTower.class, 1)) {
                return BTNode.Status.FAILURE;
            }

            actions.add(new S3Action(peasant.entityID,S3Action.ACTION_BUILD, WTower.class.getSimpleName(), loc.m_a, loc.m_b));
        }

        return BTNode.Status.SUCCESS;
    }

    private static java.util.List<s3.util.Pair<Integer,Integer>>
    ringAround(S3 g, int x, int y, int w, int h) {
        java.util.ArrayList<s3.util.Pair<Integer,Integer>> r = new java.util.ArrayList<>();
        for (int i = x - 1; i <= x + w; i++) {
            for (int j = y - 1; j <= y + h; j++) {
                boolean inside = (i >= x && i < x + w && j >= y && j < y + h);
                if (!inside && inBounds(g, i, j)) r.add(new s3.util.Pair<>(i, j));
            }
        }
        return r;
    }

    private static WGoldMine nearestMine(S3 game, WPlayer player) {
        java.util.List<WUnit> mines = game.getUnitTypes(null, WGoldMine.class);
        if (mines.isEmpty()) return null;
        // reference = nearest peasant; fallback to our townhall; else just pick first
        int rx, ry;
        var peasants = game.getUnitTypes(player, WPeasant.class);
        if (!peasants.isEmpty()) { rx = peasants.get(0).getX(); ry = peasants.get(0).getY(); }
        else {
            WTownhall th = (WTownhall) game.getUnitType(player, WTownhall.class);
            if (th != null) { rx = th.getX(); ry = th.getY(); }
            else { WUnit m0 = mines.get(0); return (WGoldMine) m0; }
        }
        WGoldMine best = null; int bestD = Integer.MAX_VALUE;
        for (WUnit u : mines) {
            int d = manhattan(rx, ry, u.getX(), u.getY());
            if (d < bestD) { bestD = d; best = (WGoldMine) u; }
        }
        return best;
    }

    private static java.util.List<s3.util.Pair<Integer,Integer>>
    ringAroundBounded(S3 g, int x, int y, int w, int h) {
        java.util.ArrayList<s3.util.Pair<Integer,Integer>> r = new java.util.ArrayList<>();
        for (int i = x - 1; i <= x + w; i++) {
            for (int j = y - 1; j <= y + h; j++) {
                boolean inside = (i >= x && i < x + w && j >= y && j < y + h);
                if (!inside && inBounds(g, i, j)) r.add(new s3.util.Pair<>(i, j));
            }
        }
        return r;
    }

    // A*-compatible yes/no, consistent with your pathfinder (8-dir, no corner cut, collision via anyLevelCollision)
    private static boolean hasAStarPassage(S3 game, int sx, int sy, int gx, int gy, s3.entities.S3PhysicalEntity mover) {
        if (!inBounds(game, sx, sy) || !inBounds(game, gx, gy)) return false;
        if (!isGrass(game, gx, gy)) return false;
        if (sx == gx && sy == gy) return true;

        int W = game.getMap().getWidth(), H = game.getMap().getHeight();
        double[][] gScore = new double[W][H];
        for (int i = 0; i < W; i++) java.util.Arrays.fill(gScore[i], Double.POSITIVE_INFINITY);
        gScore[sx][sy] = 0.0;

        class Node implements Comparable<Node> {
            int x, y; double g, f; Node p;
            Node(int x,int y,double g,double h,Node p){this.x=x;this.y=y;this.g=g;this.f=g+h;this.p=p;}
            public int compareTo(Node o){ return Double.compare(this.f, o.f); }
        }
        java.util.PriorityQueue<Node> open = new java.util.PriorityQueue<>();
        open.add(new Node(sx, sy, 0.0, Math.hypot(gx - sx, gy - sy), null));
        final double DIAG = Math.sqrt(2.0);

        int[] dX = {  0,  0,  1, -1,  1,  1, -1, -1 };
        int[] dY = {  1, -1,  0,  0,  1, -1,  1, -1 };

        while (!open.isEmpty()) {
            Node cur = open.poll();
            if (cur.g > gScore[cur.x][cur.y]) continue;
            if (cur.x == gx && cur.y == gy) return true;

            for (int i = 0; i < 8; i++) {
                int nx = cur.x + dX[i], ny = cur.y + dY[i];
                if (!inBounds(game, nx, ny) || !isGrass(game, nx, ny)) continue;

                // no diagonal corner-cutting (orthogonals must be grass)
                if (nx != cur.x && ny != cur.y) {
                    if (!isGrass(game, cur.x, ny) || !isGrass(game, nx, cur.y)) continue;
                }

                boolean blocked = false;
                if (mover != null) {
                    int ox = mover.getX(), oy = mover.getY();
                    mover.setX(nx); mover.setY(ny);
                    blocked = (game.anyLevelCollision(mover) != null);
                    mover.setX(ox); mover.setY(oy);
                }
                if (blocked) continue;

                double step = (nx != cur.x && ny != cur.y) ? DIAG : 1.0;
                double ng = cur.g + step;
                if (ng < gScore[nx][ny]) {
                    gScore[nx][ny] = ng;
                    open.add(new Node(nx, ny, ng, Math.hypot(gx - nx, gy - ny), cur));
                }
            }
        }
        return false;
    }

    private static long key(int x, int y) {
        return ((x & 0xffffffffL) << 32) | (y & 0xffffffffL);
    }

    private static int countReachableRingSlots(S3 game, WPlayer player, java.util.List<int[]> standRing) {

        if (standRing == null || standRing.isEmpty()) return 0;

        // Ring membership
        java.util.HashSet<Long> ring = new java.util.HashSet<>();
        for (int[] r : standRing) {
            if (r != null && r.length >= 2 && inBounds(game, r[0], r[1])) {
                ring.add(key(r[0], r[1]));
            }
        }
        if (ring.isEmpty()) return 0;

        // BFS setup
        final int W = mapW(game), H = mapH(game);
        boolean[][] seen = new boolean[W][H];
        java.util.ArrayDeque<int[]> q = new java.util.ArrayDeque<>();
        java.util.HashSet<Long> counted = new java.util.HashSet<>();

        // Helper: enqueue a tile if valid
        java.util.function.BiConsumer<Integer,Integer> trySeed = (sx, sy) -> {
            if (inBounds(game, sx, sy) && isGrass(game, sx, sy) && !seen[sx][sy]) {
                seen[sx][sy] = true;
                q.add(new int[]{sx, sy});
            }
        };

        // Seed from peasants (allow using 4-neighbors if the peasant tile itself isn't grass)
        java.util.List<WUnit> peasants = game.getUnitTypes(player, s3.entities.WPeasant.class);
        for (WUnit u : peasants) {
            int px = u.getX(), py = u.getY();
            if (isGrass(game, px, py)) {
                trySeed.accept(px, py);
            } else {
                // be forgiving: seed its 4-neighbors
                trySeed.accept(px + 1, py);
                trySeed.accept(px - 1, py);
                trySeed.accept(px, py + 1);
                trySeed.accept(px, py - 1);
            }
        }

        // Also seed from grass tiles around each townhall (helps early game / blocked peasants)
        for (WUnit th : game.getUnitTypes(player, s3.entities.WTownhall.class)) {
            // 4-neighbors
            trySeed.accept(th.getX() + 1, th.getY());
            trySeed.accept(th.getX() - 1, th.getY());
            trySeed.accept(th.getX(), th.getY() + 1);
            trySeed.accept(th.getX(), th.getY() - 1);
        }

        if (q.isEmpty()) {
            // No reachable grass near any seed; nothing to do.
            return 0;
        }

        // Use a throwaway probe (don’t mutate a live unit)
        s3.entities.WPeasant probe = new s3.entities.WPeasant();
        probe.setEntityID(-1); // ensure not equal to any real unit id

        // 8 directions, forbid diagonal corner cutting
        final int[] dX = {  0,  0,  1, -1,  1,  1, -1, -1 };
        final int[] dY = {  1, -1,  0,  0,  1, -1,  1, -1 };

        int found = 0;
        while (!q.isEmpty() && found < MIN_MINE_SLOTS) {
            int[] cur = q.pollFirst();
            int cx = cur[0], cy = cur[1];

            long k = key(cx, cy);
            if (ring.contains(k) && counted.add(k)) {
                if (++found >= MIN_MINE_SLOTS) break;
            }

            for (int i = 0; i < 8; i++) {
                int nx = cx + dX[i], ny = cy + dY[i];
                if (!inBounds(game, nx, ny) || seen[nx][ny]) continue;
                if (!isGrass(game, nx, ny)) continue;

                // No diagonal corner cutting
                if (nx != cx && ny != cy) {
                    if (!isGrass(game, cx, ny) || !isGrass(game, nx, cy)) continue;
                }

                // Collision against map/units with a probe
                int ox = probe.getX(), oy = probe.getY();
                probe.setX(nx); probe.setY(ny);
                s3.entities.S3Entity hit = game.anyLevelCollision(probe);
                probe.setX(ox); probe.setY(oy);

                boolean blocked = (hit != null) && !isDynamicUnit(hit);
                if (blocked) continue;

                seen[nx][ny] = true;
                q.addLast(new int[]{nx, ny});
            }
        }

        return found;
    }


    private boolean cond_NearestMineBlocked(S3 game, WPlayer player) {

        WGoldMine m = nearestMine(game, player);
        if (m == null) return false;

        var ring = ringAroundBounded(game, m.getX(), m.getY(), m.getWidth(), m.getLength());

        boolean anyTree = false;
        java.util.ArrayList<int[]> standRing = new java.util.ArrayList<>();

        // Use a single probe to filter ring tiles that are standable now
        s3.entities.S3PhysicalEntity probe = null;
        var peasants = game.getUnitTypes(player, s3.entities.WPeasant.class);
        if (!peasants.isEmpty()) probe = (s3.entities.S3PhysicalEntity) peasants.get(0);

        for (var t : ring) {
            int x = t.m_a, y = t.m_b;
            if (isTree(game, x, y)) anyTree = true;
            if (!isGrass(game, x, y)) continue;

            boolean collides = false;
            if (probe != null) {
                int ox = probe.getX(), oy = probe.getY();
                probe.setX(x); probe.setY(y);
                S3Entity hit = game.anyLevelCollision(probe);
                collides = (hit != null) && !isDynamicUnit(hit);
                probe.setX(ox); probe.setY(oy);
            }
            if (!collides) standRing.add(new int[]{x, y});
        }

        // No standable tiles at all ⇒ only call it blocked if trees are the cause
        if (standRing.isEmpty()) {
            System.out.println("No standable tiles | anyTree => " + anyTree);
            return anyTree;
        }

        // Fast count with early exit
        int reachableSlots = countReachableRingSlots(game, player, standRing);

        // If there are no trees, chopping cannot help ⇒ NOT blocked.
        if (!anyTree) return false;

        // Otherwise, require ≥ MIN_MINE_SLOTS reachable tiles
        boolean blocked = (reachableSlots < MIN_MINE_SLOTS);

        if (blocked) {
            System.out.println("reachableSlots => " + reachableSlots);
        }

        return blocked;
    }


    /*
     * (non-Javadoc)
     *
     * @see base.ai.AI#getPlayerId()
     */
    public String getPlayerId() {
        return m_playerID;
    }

}
