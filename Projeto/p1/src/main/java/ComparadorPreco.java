/**
 * Comparator da classe veículo, que compara dois veículos
 * através do preço base por Km de cada um.
 *
 * 
 * 
 * 
 * @version 20190415
 */

import java.util.Comparator;

public class ComparadorPreco implements Comparator<Veiculo>{
    public int compare(Veiculo v1, Veiculo v2) {
        if(v1.getPreco() > v2.getPreco()) {
            return 1;
        }
        return -1;
    }
}
