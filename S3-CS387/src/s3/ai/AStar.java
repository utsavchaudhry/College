package s3.ai;

import java.util.List;
import java.util.PriorityQueue;
import java.util.LinkedList;
import java.util.Arrays;
import s3.base.S3;
import s3.entities.S3Entity;
import s3.entities.S3PhysicalEntity;
import s3.entities.WOGrass;
import s3.util.Pair;

public class AStar {

	private static final double DIAG_COST = Math.sqrt(2.0);

	private static class Node implements Comparable<Node> {
		int x, y;
		double g;      // cost from start to this node
		double f;      // estimated total cost (g + h)
		Node parent;   // backpointer for path reconstruction

		Node(int x, int y, double g, double h, Node parent) {
			this.x = x;
			this.y = y;
			this.g = g;
			this.f = g + h;
			this.parent = parent;
		}

		@Override
		public int compareTo(Node other) {
			return Double.compare(this.f, other.f);
		}
	}

	/**
	 * Static helper to get the number of steps in a path (for AI distance queries).
	 * Returns -1 if no path exists.
	 */
	public static int pathDistance(double start_x, double start_y,
								   double goal_x, double goal_y,
								   S3PhysicalEntity entity, S3 game) {
		AStar planner = new AStar(start_x, start_y, goal_x, goal_y, entity, game);
		List<Pair<Double, Double>> path = planner.computePath();
		return (path != null ? path.size() : -1);
	}

	// Grid‐aligned start and goal coordinates
	private final int startX, startY, goalX, goalY;
	private final S3PhysicalEntity entity;
	private final S3 game;

	public AStar(double sx, double sy, double gx, double gy,
				 S3PhysicalEntity entity, S3 game) {
		this.startX = (int) Math.floor(sx);
		this.startY = (int) Math.floor(sy);
		this.goalX  = (int) Math.floor(gx);
		this.goalY  = (int) Math.floor(gy);
		this.entity = entity;
		this.game   = game;
	}

	/**
	 * Computes and returns the path from start→goal as a list of (x,y) pairs.
	 * Excludes the start cell, includes the goal cell. Returns null if unreachable.
	 */
	public List<Pair<Double, Double>> computePath() {

		if (startX == goalX && startY == goalY) {
			return new LinkedList<>();
		}

		if (!(game.getEntity(goalX, goalY) instanceof WOGrass)) {
			return null;
		}

		int width  = game.getMap().getWidth();
		int height = game.getMap().getHeight();

		// gScore holds best‐found cost to each cell
		double[][] gScore = new double[width][height];
		for (double[] row : gScore) {
			Arrays.fill(row, Double.POSITIVE_INFINITY);
		}
		gScore[startX][startY] = 0.0;

		// Open list ordered by f = g + h
		PriorityQueue<Node> open = new PriorityQueue<>();
		open.add(new Node(startX, startY, 0.0, heuristic(startX, startY), null));

		// Offsets for 8‐direction movement
		int[] dX = {  0,  0,  1, -1,  1,  1, -1, -1 };
		int[] dY = {  1, -1,  0,  0,  1, -1,  1, -1 };

		Node reachedGoal = null;

		while (!open.isEmpty()) {
			Node cur = open.poll();
			if (cur.g > gScore[cur.x][cur.y]) continue;
			if (cur.x == goalX && cur.y == goalY) {
				reachedGoal = cur;
				break;
			}

			// Explore neighbors
			for (int i = 0; i < 8; i++) {
				int nx = cur.x + dX[i];
				int ny = cur.y + dY[i];

				// Bounds check
				if (nx < 0 || ny < 0 || nx >= width || ny >= height) continue;

				// Prevent corner‐cutting on diagonals
				if (nx != cur.x && ny != cur.y) {
					S3Entity e1 = game.getEntity(cur.x, ny);
					S3Entity e2 = game.getEntity(nx, cur.y);
					if (!(e1 instanceof WOGrass) || !(e2 instanceof WOGrass)) {
						continue;
					}
				}

				// Collision check via anyLevelCollision
				int oldX = entity.getX(), oldY = entity.getY();
				entity.setX(nx);
				entity.setY(ny);
				boolean blocked = (game.anyLevelCollision(entity) != null);
				entity.setX(oldX);
				entity.setY(oldY);
				if (blocked) continue;

				// Compute move cost (straight=1, diagonal=√2)
				double moveCost = (nx != cur.x && ny != cur.y) ? DIAG_COST : 1.0;
				double tentativeG = cur.g + moveCost;
				if (tentativeG < gScore[nx][ny]) {
					gScore[nx][ny] = tentativeG;
					double h = heuristic(nx, ny);
					open.add(new Node(nx, ny, tentativeG, h, cur));
				}
			}
		}

		// If no path found
		if (reachedGoal == null) {
			return null;
		}

		// Reconstruct: walk back from goal to start (excluding start)
		LinkedList<Pair<Double, Double>> path = new LinkedList<>();
		for (Node n = reachedGoal; n.parent != null; n = n.parent) {
			path.addFirst(new Pair<>((double) n.x, (double) n.y));
		}
		return path;
	}

	private double heuristic(int x, int y) {
		double dx = goalX - x;
		double dy = goalY - y;
		return Math.hypot(dx, dy);
	}
}
