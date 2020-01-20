//import controller.Controller;
//import model.Parser;
//import model.UMCarroJa;

import java.io.IOException;
import java.io.FileWriter;
import java.io.File;
import java.util.logging.Logger;

public class Main {

    private static final Logger logger = Logger.getLogger(Main.class.getName());

    public static void main(String[] args) {
        FileWriter fw;

        UMCarroJa model = new UMCarroJa();
        double[] before = EnergyCheckUtils.getEnergyStats();
         
        try {
            model = UMCarroJa.read(".tmp");
        } catch (IOException | ClassNotFoundException e) {
            new Parser("db/Exemplo.bak", model);
        }

        double[] after = EnergyCheckUtils.getEnergyStats();  
            
        try {
            fw = new FileWriter(new File("output.txt"));

            fw.write("Energy consumption of dram: " + (after[0] - before[0]) +
                         " Energy consumption of cpu: " + (after[1] - before[1]) +
                           " Energy consumption of package: " + (after[2] - before[2]));
            fw.close();
        } catch (IOException e) {
            System.out.println(e);
        }

        try {
            Thread.sleep(30);
        } catch (Exception e) {
            logger.warning(e.toString());
        }

        new Controller(model).run();

        try {
            model.save(".tmp");
        } catch (IOException ignored) {
            logger.warning("Not saved!");
        }
    }
}
