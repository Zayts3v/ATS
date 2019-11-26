/**
 * Comparator da classe cliente, que compara dois clientes
 * através do número de Km's efetuados na aplicação.
 *
 * 
 * 
 * 
 * @version 20190415
 */

import java.util.Comparator;

public class ComparadorKm implements Comparator<Cliente> {
    public int compare(Cliente c1, Cliente c2) {
        if(c1.getNKm() < c2.getNKm()) {
            return 1;
        }
        return -1;
    }
}

