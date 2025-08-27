package s3.ai.builtin.files_uc49;

import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;

import org.jdom.Document;
import org.jdom.Element;
import org.jdom.input.SAXBuilder;
import s3.ai.builtin.utsav_uc49;

public class BTTreeLoader {

    public static BTNode load(utsav_uc49 ai) {

        String resourcePath = "bt_uc49.xml";

        try {
            InputStream is = BTTreeLoader.class
                    .getResourceAsStream(resourcePath);
            if (is == null) {
                throw new RuntimeException("BT XML not found: " + resourcePath);
            }

            SAXBuilder builder = new SAXBuilder();
            Document doc = builder.build(is);
            Element root = doc.getRootElement();
            // If wrapped in <behaviorTree>, unwrap it
            if ("behaviorTree".equals(root.getName())) {
                for (Object o : root.getChildren()) {
                    if (o instanceof Element) {
                        root = (Element)o;
                        break;
                    }
                }
            }
            return parseElement(root, ai);
        } catch (Exception e) {
            throw new RuntimeException("Error loading BT", e);
        }
    }

    private static BTNode parseElement(Element elem, utsav_uc49 ai) {
        String tag = elem.getName();
        switch (tag) {
            case "selector": {
                List<BTNode> kids = new ArrayList<>();
                for (Object o : elem.getChildren()) {
                    if (o instanceof Element) {
                        kids.add(parseElement((Element)o, ai));
                    }
                }
                return new BTSelector(kids);
            }
            case "sequence": {
                List<BTNode> kids = new ArrayList<>();
                for (Object o : elem.getChildren()) {
                    if (o instanceof Element) {
                        kids.add(parseElement((Element)o, ai));
                    }
                }
                return new BTSequence(kids);
            }
            case "condition":
                return new BTCondition(elem.getText(), ai);
            case "action":
                return new BTAction(elem.getText(), ai);
            default:
                throw new RuntimeException("Unknown BT tag: " + tag);
        }
    }
}
