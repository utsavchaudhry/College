package s3.ai.builtin.files_uc49;

import java.util.List;
import s3.base.S3;
import s3.entities.WPlayer;
import s3.base.S3Action;

/**
 * ticks children in order, returns FAILURE on first child
 * that returns FAILURE; RUNNING if a child is RUNNING;
 * SUCCESS if all children succeed.
 */
public class BTSequence extends BTNode {
    private final List<BTNode> children;
    private int runningIndex = 0;

    public BTSequence(List<BTNode> children) {
        this.children = children;
    }

    @Override
    public Status tick(S3 game, WPlayer player, List<S3Action> actions) {
        for (int i = runningIndex; i < children.size(); i++) {
            Status result = children.get(i).tick(game, player, actions);
            if (result == Status.FAILURE) {
                runningIndex = 0;
                return Status.FAILURE;
            }
            if (result == Status.RUNNING) {
                runningIndex = i;
                return Status.RUNNING;
            }
            // else SUCCESS -> continue
        }
        runningIndex = 0;
        return Status.SUCCESS;
    }
}
