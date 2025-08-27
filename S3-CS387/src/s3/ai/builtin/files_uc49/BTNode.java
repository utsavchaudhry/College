package s3.ai.builtin.files_uc49;

import s3.base.S3;
import s3.entities.WPlayer;
import s3.base.S3Action;
import java.util.List;

public abstract class BTNode {
    public enum Status { SUCCESS, FAILURE, RUNNING }

    public abstract Status tick(S3 game, WPlayer player, List<S3Action> actions);
}
