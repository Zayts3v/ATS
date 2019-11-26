/**
 * Interface relativa à classificação de clientes / veículos.
 * 
 */

public interface Classificação{
    
    /**
     * Método que devolve a classificação.
     * @return int Representa a classificação.
     */
    int getClassificacao();
    
    /**
     * Método que altera o valor do campo classificação.
     * @param classificacao Classificação nova.
     */
    void setClassificacao(int classificacao);
}
