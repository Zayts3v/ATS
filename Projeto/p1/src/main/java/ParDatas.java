/**
 * Classe que representa um par de datas.
 * 
 * 
 * 
 * 
 * @version 20190415
 */

import java.io.Serializable;
import java.util.GregorianCalendar;

public class ParDatas implements Serializable {
    
    /* Data de início do evento */
    private GregorianCalendar dataInicio;
    /* Data de fim do evento */
    private GregorianCalendar dataFim;
    
    /**
     * Construtor por omissão.
     * É criada uma instância da classe ParDatas.
     */
    public ParDatas(){
        this.dataInicio = new GregorianCalendar();
        this.dataFim = new GregorianCalendar();
    }
    
    /**
     * Construtor por parâmetro.
     * @param beggining  Data de início do evento.
     * @param end        Data de início do evento.
     */
    public ParDatas(GregorianCalendar beginning, GregorianCalendar end){
        this.dataInicio = beginning;
        this.dataFim = end;
    }
    
    /**
     * Construtor por cópia.
     * @param outroVeiculo Um outro veículo.
     */
    public ParDatas(ParDatas outroPar){
        this.dataInicio = outroPar.getDataInicio();
        this.dataFim = outroPar.getDataFim();
    }
    
    /*****************************************************************************
     *                             BEGIN GETTERS                                 *
     *****************************************************************************/
    
    public GregorianCalendar getDataInicio(){
        return this.dataInicio;
    }
    
    public GregorianCalendar getDataFim(){
        return this.dataFim;
    }
    
    /*****************************************************************************
     *                              END GETTERS                                  *
     *****************************************************************************/
     
    /*****************************************************************************
     *                             BEGIN SETTERS                                 *
     *****************************************************************************/
     
    public void setDataInicio(GregorianCalendar inicio){
        this.dataInicio = inicio;
    }
    
    public void setDataFim(GregorianCalendar fim){
        this.dataFim = fim;
    }
    
    /*****************************************************************************
     *                              END SETTERS                                  *
     *****************************************************************************/
    
    public boolean isAvailable(ParDatas dataPedido){
        GregorianCalendar dataPedidoI = dataPedido.getDataInicio();
        GregorianCalendar dataPedidoF = dataPedido.getDataFim();
        /* Quando a data de pedido do aluguer esta dentro do periodo de um aluguer */
        if(this.dataInicio.before(dataPedidoI) && this.dataFim.after(dataPedidoF) ){
            return false;
        }
        /* Quando a data de pedido do aluguer se inicia durante o periodo de um alguer mas acaba depois do fim do periodo de aluguer  */
        if (this.dataInicio.before(dataPedidoI) && this.dataFim.after(dataPedidoI)){
            return false;    
        }
        /* Quando a data de pedido de aluguer se inicia antes de um periodo de aluger mas acaba durante um periodo de aluguer*/
        if (dataPedidoI.before(this.dataInicio) && dataPedidoF.before(this.dataFim) ){       
            return false;
        }else {
            return true;
        }
    }
    
    /**
     * Compara a igualdade com outro objeto.
     * @param obj O objeto a comparar.
     * @return true se forem iguais, false caso contrário.
     */
    public boolean equals(Object obj) {
        if(obj==this) return true;
        if(obj==null || obj.getClass()!=this.getClass()) return false;
        ParDatas date = (ParDatas) obj;
        return this.dataInicio.equals(date.getDataInicio()) && this.dataFim.equals(date.getDataFim());
    }
    
    /** 
     * Devolve uma representação do objeto em formato textual.
     * @return String Representa o objeto em formato textual.
     */
    public String toString() {
       StringBuilder str = new StringBuilder();
       
       str.append("Data de início: "); str.append(this.dataInicio); str.append("\n");
       str.append("Data de fim: "); str.append(this.dataFim); str.append("\n");
        
       return str.toString();         
    }
    
    /**
     * Retorna uma cópia da instância.
     * @return ParDatas Um novo par de datas que é cópia do this.
     */
    public ParDatas clone(){
        return new ParDatas(this);
    }
}
