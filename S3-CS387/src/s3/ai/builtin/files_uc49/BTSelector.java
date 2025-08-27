package s3.ai.builtin.files_uc49;

import java.util.List;
import s3.base.S3;
import s3.entities.WPlayer;
import s3.base.S3Action;

/**
 * ticks children in order, returns SUCCESS on first child
 * that returns SUCCESS; RUNNING if a child is RUNNING;
 * FAILURE if all children fail.
 */
public class BTSelector extends BTNode {
    private final List<BTNode> children;
    private int runningIndex = 0;

    public BTSelector(List<BTNode> children) {
        this.children = children;
    }

    @Override
    public Status tick(S3 game, WPlayer player, List<S3Action> actions) {
        for (int i = runningIndex; i < children.size(); i++) {
            Status result = children.get(i).tick(game, player, actions);
            if (result == Status.SUCCESS) {
                runningIndex = 0;
                return Status.SUCCESS;
            }
            if (result == Status.RUNNING) {
                runningIndex = i;
                return Status.RUNNING;
            }
            // else FAILURE -> try next child
        }
        runningIndex = 0;
        return Status.FAILURE;
    }
}
