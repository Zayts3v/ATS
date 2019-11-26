/**
 * Comparator da classe veículo, que compara dois veículos
 * através da sua autonomia.
 *
 * 
 * 
 * 
 * @version 20190415
 */

import java.util.Comparator;

public class ComparadorAutonomia implements Comparator<Veiculo> {
    public int compare(Veiculo a1, Veiculo a2) {
        if(a1.getAutonomia() > a2.getAutonomia()) {
            return 1;
        }
        return -1;
    }
}