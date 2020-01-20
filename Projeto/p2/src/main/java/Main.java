import controller.Controller;
import model.Parser;
import model.UMCarroJa;

import java.io.IOException;
import java.util.logging.Logger;


public class Main {

    private static final Logger logger = Logger.getLogger(Main.class.getName());

    public static void main(String[] args) {
        UMCarroJa model = new UMCarroJa();
        try {
            model = UMCarroJa.read("db/logsPOO_carregamentoInicial.bak");
        }
        catch (IOException | ClassNotFoundException e) {
            new Parser("db/logsPOO_carregamentoInicial.bak", model);
        }
        try { Thread.sleep(10000);} catch (Exception e) { logger.warning(e.toString()); }
        new Controller(model).run();
        try {
            model.save(".tmp");
        }
        catch (IOException ignored) { logger.warning("Not saved!"); }
    }
}
