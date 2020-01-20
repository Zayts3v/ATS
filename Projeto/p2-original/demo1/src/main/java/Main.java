//import Controller.Controller;
//import Model.Parser;
//import Model.UMCarroJa;

import java.io.IOException;
import java.io.FileWriter;
import java.io.File;

public class Main {
    public static void main(String[] args) {
        FileWriter fw;
        UMCarroJa model = new UMCarroJa();
        double[] before = EnergyCheckUtils.getEnergyStats();

        try {
            model = UMCarroJa.read(".tmp");
            System.out.println("adasdsada1");
        }
        catch (IOException | ClassNotFoundException e) {
            System.out.println("adasdsada2");
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

        try { Thread.sleep(10000);} catch (Exception e) {}
        new Controller(model).run();
        try {
            model.save(".tmp");
        }
        catch (IOException ignored) {}
    }
}
