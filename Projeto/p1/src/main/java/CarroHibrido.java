/**
 * Classe que extende a classe veículo e que
 * representa um carro híbrido.
 * 
 */

import java.util.List;

public class CarroHibrido extends Veiculo{
    
    /**
     * Construtor por omissão.
     * É criada uma instância da classe CarroHibrido.
     */
    public CarroHibrido(){
        super();
    }
    
    /**
     * Construtor por parâmetro.
     * @param marca       Marca do carro híbrido.
     * @param matricula   Matrícula do carro híbrido.
     * @param nif         NIF do proprietário do carro híbrido.
     * @param velocidade  Velocidade média por Km do carro híbrido.
     * @param preco       Preço base por cada Km percorrido do carro híbrido.
     * @param consumo     Consumo por Km percorrido do carro híbrido. 
     * @param autonomia   Autonomia em Km do carro híbrido.
     * @param cords       Coordenadas em que o carro híbrido se encontra.
     * @param disponivel  True se o carro híbrido estiver disponível, false caso contrário.
     * @param classif     Classificação do carro híbrido.
     * @param datasAlugs  Datas dos alugueres do carro híbrido.
     */
    public CarroHibrido(String marca, String matricula, String nif, int velocidade, double preco, double consumo, 
                        int autonomia, Coordinate cords, boolean disponivel, int classif, List<ParDatas> datasAlugs){
        super(marca, matricula, nif, velocidade, preco, consumo, autonomia, cords, disponivel, classif, datasAlugs);       
    }
    
    /**
     * Construtor por cópia.
     * @param ch Um outro carro híbrido.
     */
    public CarroHibrido(CarroHibrido ch){
        super(ch);       
    }
        
    /**
     * Compara a igualdade com outro objeto.
     * @param obj O objeto a comparar.
     * @return true se forem iguais, false caso contrário.
     */
    public boolean equals(Object obj) {
        if(obj==this) return true;
        if(obj==null || obj.getClass()!=this.getClass()) return false;
        CarroHibrido ch = (CarroHibrido) obj;
        return(super.equals(ch));
    }
    
    /** 
     * Devolve uma representação do objeto em formato textual.
     * @return String Representa o objeto em formato textual.
     */
    public String toString() {
        StringBuilder str = new StringBuilder();
        str.append("***** Carro Híbrido *****\n");
        str.append(super.toString());
        return str.toString();
    }
    
    /**
     * Retorna uma cópia da instância.
     * @return CarroHibrido Um novo carro híbrido, que é cópia do this.
     */
    public CarroHibrido clone(){
        return new CarroHibrido(this);
    }  
}