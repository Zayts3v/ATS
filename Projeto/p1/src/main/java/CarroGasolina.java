/**
 * Classe que extende a classe veículo e que
 * representa um carro a gasolina.
 * 
 */

import java.util.List;

public class CarroGasolina extends Veiculo{
    
    /**
     * Construtor por omissão.
     * É criada uma instância da classe CarroGasolina.
     */
    public CarroGasolina(){
        super();
    }
    
    /**
     * Construtor por parâmetro.
     * @param marca       Marca do carro a gasolina.
     * @param matricula   Matrícula do carro a gasolina.
     * @param nif         NIF do proprietário do carro a gasolina.
     * @param velocidade  Velocidade média por Km do carro a gasolina.
     * @param preco       Preço base por cada Km percorrido do carro a gasolina.
     * @param consumo     Consumo por Km percorrido do carro a gasolina. 
     * @param autonomia   Autonomia em Km do carro a gasolina.
     * @param cords       Coordenadas em que o carro a gasolina se encontra.
     * @param disponivel  True se o carro a gasolina estiver disponível, false caso contrário.
     * @param classif     Classificação do carro a gasolina.
     * @param datasAlugs  Datas dos alugueres do carro a gasolina.
     */
    public CarroGasolina(String marca, String matricula, String nif, int velocidade, double preco, double consumo, 
                         int autonomia, Coordinate cords, boolean disponivel, int classif, List<ParDatas> datasAlugs){
        super(marca, matricula, nif, velocidade, preco, consumo, autonomia, cords, disponivel, classif, datasAlugs);              
    }
    
    /**
     * Construtor por cópia.
     * @param cg
     */
    public CarroGasolina(CarroGasolina cg){
        super(cg);       
    }
        
    /**
     * Compara a igualdade com outro objeto.
     * @param obj O objeto a comparar.
     * @return true se forem iguais, false caso contrário.
     */
    public boolean equals(Object obj) {
        if(obj==this) return true;
        if(obj==null || obj.getClass()!=this.getClass()) return false;
        CarroGasolina cg = (CarroGasolina) obj;
        return(super.equals(cg));
    }
    
    /** 
     * Devolve uma representação do objeto em formato textual.
     * @return String Representa o objeto em formato textual.
     */
    public String toString() {
        StringBuilder str = new StringBuilder();
        str.append("***** Carro a Gasolina *****\n");
        str.append(super.toString());
        return str.toString();
    }
    
    /**
     * Retorna uma cópia da instância.
     * @return CarroGasolina Um novo carro a gasolina, que é cópia do this.
     */
    public CarroGasolina clone(){
        return new CarroGasolina(this);
    }  
}
