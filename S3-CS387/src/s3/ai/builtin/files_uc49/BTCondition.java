package s3.ai.builtin.files_uc49;

import s3.ai.builtin.utsav_uc49;
import s3.base.S3;
import s3.entities.WPlayer;
import s3.base.S3Action;
import java.util.List;

public class BTCondition extends BTNode {
    private final String name;
    private utsav_uc49 ai;

    public BTCondition(String name, utsav_uc49 ai) {
        this.name = name.trim();
        this.ai = ai;
    }

    @Override
    public Status tick(S3 game, WPlayer player, List<S3Action> actions) {
        boolean ok = ai.evaluateConditionStatic(name, game, player);
        System.out.println(name + ": " + ok);
        return ok ? Status.SUCCESS : Status.FAILURE;
    }
}
