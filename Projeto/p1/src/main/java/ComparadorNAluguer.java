/**
 * Comparator da classe cliente, que compara dois clientes
 * através do número de alugueres efetuados na aplicação.
 *
 * 
 * 
 * 
 * @version 20190415
 */

import java.util.Comparator;

public class ComparadorNAluguer implements Comparator<Cliente>{
    public int compare(Cliente c1, Cliente c2) {
        if(c1.getNAlugueres() < c2.getNAlugueres()) {
            return 1;
        }
        return -1;
    }
}