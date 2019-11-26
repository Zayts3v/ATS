/**
 * Classe que extende a classe veículo e que
 * representa um carro elétrico.
 * 
 */

import java.util.List;

public class CarroEletrico extends Veiculo{
    
    /**
     * Construtor por omissão.
     * É criada uma instância da classe CarroEletrico.
     */
    public CarroEletrico(){
        super();
    }
    
    /**
     * Construtor por parâmetro.
     * @param marca       Marca do carro elétrico.
     * @param matricula   Matrícula do carro elétrico.
     * @param nif         NIF do proprietário do carro elétrico.
     * @param velocidade  Velocidade média por Km do carro elétrico.
     * @param preco       Preço base por cada Km percorrido do carro elétrico.
     * @param consumo     Consumo por Km percorrido do carro elétrico. 
     * @param autonomia   Autonomia em Km do carro elétrico.
     * @param cords       Coordenadas em que o carro elétrico se encontra.
     * @param disponivel  True se o carro elétrico estiver disponível, false caso contrário.
     * @param classif     Classificação do carro elétrico.
     * @param datasAlugs  Datas dos alugueres do carro a gasolina.
     */
    public CarroEletrico(String marca, String matricula, String nif, int velocidade, double preco, double consumo, 
                         int autonomia, Coordinate cords, boolean disponivel, int classif, List<ParDatas> datasAlugs){
        super(marca, matricula, nif, velocidade, preco, consumo, autonomia, cords, disponivel, classif, datasAlugs);              
    }
    
    /**
     * Construtor por cópia.
     * @param ce Um outro carro elétrico.
     */
    public CarroEletrico(CarroEletrico ce){
        super(ce);       
    }
        
    /**
     * Compara a igualdade com outro objeto.
     * @param obj O objeto a comparar.
     * @return true se forem iguais, false caso contrário.
     */
    public boolean equals(Object obj) {
        if(obj==this) return true;
        if(obj==null || obj.getClass()!=this.getClass()) return false;
        CarroEletrico ce = (CarroEletrico) obj;
        return(super.equals(ce));
    }
    
    /** 
     * Devolve uma representação do objeto em formato textual.
     * @return String Representa o objeto em formato textual.
     */
    public String toString() {
        StringBuilder str = new StringBuilder();
        str.append("***** Carro Elétrico *****\n");
        str.append(super.toString());
        return str.toString();
    }
    
    /**
     * Retorna uma cópia da instância.
     * @return CarroEletrico Um novo carro elétrico, que é cópia do this.
     */
    public CarroEletrico clone(){
        return new CarroEletrico(this);
    }  
}