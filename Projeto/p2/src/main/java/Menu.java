//package view;

//import exceptions.InvalidNewRegisterException;
//import exceptions.InvalidNewRentalException;
//import exceptions.InvalidRatingException;
//import exceptions.InvalidTimeIntervalException;
//import utils.Point;
//import utils.StringBetter;
//import view.viewmodel.*;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;
import java.util.*;

import static java.lang.System.out;

public class Menu{
    private MenuInd menuOpts;
    private final Deque<MenuInd> prev;
    private final ArrayList<MenuInd> options;
    private boolean run;

    private final Scanner scanner = new Scanner(System.in);
    private static final String CLEAR = "\033\143";
    private static final String CARTYPE = "Tipo do Carro: [electric, gas, hybrid or any]";

    public enum MenuInd {
        INITIAL,
        LOGIN,
        REGISTER,
        REGISTERCLIENT,
        REGISTEROWNER,
        CLIENT,
        OWNER,
        HISTORYCLIENT,
        CLOSEST,
        CHEAPEST,
        CHEAPESTNEAR,
        SPECIFIC,
        AUTONOMY,
        TOP10CLIENTS,
        HISTORYOWNER,
        REVIEWRENTAL,
        CAROVERVIEW,
        ADDCAR,
        RENTALS,
        PENDING,
        NUSES,
        DISTANCE
    }

    public Menu() {
        this.menuOpts = MenuInd.INITIAL;
        this.prev = new ArrayDeque<>();
        this.options = new ArrayList<>();
        this.run = true;
        this.pickChildMenus();
    }

    public MenuInd getMenuOpts() {
        return this.menuOpts;
    }

    public void showString(String rental) {
        out.print(CLEAR);
        out.println(this.createHeader());
        out.println();
        out.println(rental);
        scanner.nextLine();
    }

    public int showRentalRate(String rental) {
        String error = "";
        while(true) {
            this.displayMenuHeader(error);
            out.println(rental);
            out.println("Client rating:");
            try {
                return scanner.nextInt();
            }
            catch (InputMismatchException e) {error = "Invalid rating";}
        }
    }

    public String carOverviewShow (String error, List<List<String>> valTab){
        this.displayMenuHeader(error);
        ArrayList<String> colLabl = new ArrayList<>();
        colLabl.add("Matricula");
        colLabl.add("Autonomia");
        colLabl.add("Preço/km");
        colLabl.add("Disponibilidade");
        colLabl.add("Ratings");

        this.tableDefault(valTab, colLabl);

        out.println("\tR[pos] -> Refill car\n\tC[pos] [price] -> Change Price\n\tD[pos] -> Toggle Availability\n\tT[pos] -> total faturado");

        return new Scanner(System.in).nextLine().toLowerCase();
    }

    public void rentalHistoryShow (TimeInterval ti, List<List<String>> valTab){
        this.displayMenuHeader("");
        ArrayList<String> colLabl = new ArrayList<>();
        colLabl.add("Data");
        colLabl.add("Carro");
        colLabl.add("Utilizador");
        colLabl.add("Inicio da Viagem");
        colLabl.add("Fim da Viagem");
        colLabl.add("Preço Final");

        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm");
        out.println(ti.getInicio().format(formatter) + " -> " + ti.getFim().format(formatter));

        tableDefault(valTab, colLabl);

        new Scanner(System.in).nextLine();
    }

    public AutonomyCar autonomyCarRent(String error) throws InvalidNewRentalException {
        this.displayMenuHeader(error);
        out.println(CARTYPE);
        String carType = scanner.nextLine();
        try {
            out.println("Alcance:");
            int range = scanner.nextInt();
            return new AutonomyCar(this.getDest(), range, carType);
        } catch (InputMismatchException e) {
            throw new InvalidNewRentalException();
        }
    }

    public CheapestNearCar walkingDistanceRent(String error) throws InvalidNewRentalException {
        this.displayMenuHeader(error);
        out.println(CARTYPE);
        String carType = scanner.nextLine();
        try {
            out.println("Distância a andar a pé:");
            int walk = scanner.nextInt();
            return new CheapestNearCar(this.getDest(), walk, carType);
        } catch (InputMismatchException e) {
            throw new InvalidNewRentalException();
        }
    }

    public String reviewRentShow(String error, int ownerRating, List<List<String>> lR){
        this.displayMenuHeader(error);
        ArrayList<String> colLabl = new ArrayList<>();
        colLabl.add("Client");
        colLabl.add("Carro");
        colLabl.add("Inicio");
        colLabl.add("Fim");
        colLabl.add("Tempo a pé");
        colLabl.add("Tempo Estimado");
        colLabl.add("Custo Estimado");
        colLabl.add("Client Rating");

        out.println("Rating pessoal: " + ownerRating);

        this.tableDefault(lR, colLabl);

        out.println("\tA[pos] -> aprove rental\n\tR[pos] -> refuse rental");

        return scanner.nextLine().toLowerCase();
    }

    public void top10ClientsShow (List<List<String>> valTab, String colum){
        ArrayList<String> colLabl = new ArrayList<>();
        colLabl.add("User");
        colLabl.add(colum);
        this.displayMenuHeader("");
        this.tableDefault(valTab, colLabl);
        new Scanner(System.in).nextLine();
    }

    public SpecificCar specificCarRent(String error) throws InvalidNewRentalException {
        this.displayMenuHeader(error);
        out.println("Matricula:");
        String carType = scanner.nextLine();
        try {
            return new SpecificCar(this.getDest(), carType);
        } catch (InputMismatchException e) {
            throw new InvalidNewRentalException();
        }
    }

    public RentCarSimple simpleCarRent(String error) throws InvalidNewRentalException {
        this.displayMenuHeader(error);
        out.println(CARTYPE);
        String carType = scanner.nextLine();
        try {
            return new RentCarSimple(this.getDest(), carType);
        } catch (InputMismatchException e) {
            throw new InvalidNewRentalException();
        }
    }

    public NewLogin newLogin(String error) {
        this.displayMenuHeader(error);
        out.println("User:");
        String user = scanner.nextLine();
        out.println("Password:");
        String password = scanner.nextLine();

        return new NewLogin(user, password);
    }

    public RegisterCar newRegisterCar(String error) throws InvalidNewRegisterException {
        this.displayMenuHeader(error);
        out.println("Matricula:");
        String matricula = scanner.nextLine();
        out.println("Marca:");
        String marca = scanner.nextLine();
        out.println("Tipo do Carro: [electric, gas or hybrid]");
        String carType = scanner.nextLine();
        try {
            out.println("Velocidade Média:");
            double avgSpeed = scanner.nextDouble();
            out.println("Preço base:");
            double basePrice = scanner.nextDouble();
            out.println("Consumo médio:");
            double gasMileage = scanner.nextDouble();
            out.println("Alcance:");
            int range = scanner.nextInt();

            return new RegisterCar(
                    matricula,
                    carType,
                    avgSpeed,
                    basePrice,
                    gasMileage,
                    range,
                    this.getDest(),
                    marca);
        } catch (InputMismatchException e) {
            throw new InvalidNewRegisterException();
        }
    }

    public RegisterUser newRegisterUser(String error) throws InvalidNewRegisterException {
        displayMenuHeader(error);
        out.println("Nome de Utilizador:");
        String user = scanner.nextLine();
        out.println("Email:");
        String email = scanner.nextLine();
        out.println("Password:");
        String pass = scanner.nextLine();
        out.println("Morada:");
        String adress = scanner.nextLine();
        int nif;
        try {
            out.println("Nif:");
            nif = scanner.nextInt();
        }
        catch (InputMismatchException e) {
            throw new InvalidNewRegisterException();
        }
        if (this.menuOpts.equals(MenuInd.REGISTERCLIENT)) {
            try {
                return new RegisterUser(user, email, pass, adress, nif, this.getLoc());
            }
            catch (InputMismatchException e) {
                throw new InvalidNewRegisterException();
            }
        }
        else {
            return new RegisterUser(user, email, pass, adress, nif);
        }


    }

    public Menu parser() {
        out.println(this);
        String str = scanner.nextLine();
        if (str.matches("^[+-]?\\d{1,8}$")) {
            int i = Integer.parseInt(str);
            if (this.options.size() > i - 1 && i > 0) {
                this.prev.addFirst(this.menuOpts);
                this.menuOpts = this.options.get(i - 1);
                this.pickChildMenus();
            }
        }
        switch (str) {
            case "b":
            case "..":
                this.back();
                break;
            case "e":
                this.run = false;
                break;
            default:
                out.println("Opção Inválida");
                break;
        }

        return this;
    }

    public Menu selectOption(MenuInd i) {
        this.prev.addFirst(this.menuOpts);
        this.menuOpts = i;
        this.pickChildMenus();
        return this;
    }

    public TimeInterval getTimeInterval(String error) throws InvalidTimeIntervalException{
        this.displayMenuHeader(error);
        try {
            DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm");

            out.println("Inicio de Intervalo (yyyy-MM-dd HH:mm):");
            LocalDateTime inicio = LocalDateTime.parse(scanner.nextLine(), formatter);

            out.println("Fim de Intervalo (yyyy-MM-dd HH:mm):");
            LocalDateTime fim = LocalDateTime.parse(scanner.nextLine(), formatter);

            return new TimeInterval(inicio, fim);
        }
        catch (DateTimeParseException e){
            throw new InvalidTimeIntervalException();
        }
    }

    public RateOwnerCar pendingRateShow(String error, String pending, int total) throws InvalidRatingException {
        displayMenuHeader(error);
        out.println(total + ".");
        out.println(pending);
        out.println();
        try {
            out.println("Rating de Owner");
            int owner = scanner.nextInt();
            if (owner < 0 || owner > 100)
                throw new InvalidRatingException();
            out.println("Rating de Carro");
            int carro = scanner.nextInt();
            if (carro < 0 || carro > 100)
                throw new InvalidRatingException();
            return new RateOwnerCar(owner, carro);
        }
        catch (InputMismatchException e){
            throw new InvalidRatingException();
        }

    }

    public boolean getRun() { return this.run; }

    public Menu back() {
        if (this.prev.isEmpty()) {
            this.menuOpts = this.prev.removeFirst();
            this.pickChildMenus();
        } else {
            this.run = false;
        }
        if (this.menuOpts.equals(MenuInd.LOGIN) || this.menuOpts.equals(MenuInd.REGISTER))
            this.back();
        return this;
    }

    private void displayMenuHeader(String error) {
        out.print(CLEAR);
        out.println(this.createHeader());
        out.println(new StringBetter(error).under().toString());
    }

    private Point getDest(){
        out.println("UMCarroJa wants to know your destination!");
        out.println("x:");
        double x = scanner.nextDouble();
        out.println("y:");
        double y = scanner.nextDouble();

        return new Point(x, y);
    }

    private Point getLoc(){
        out.println("UMCarroJa wants to know your location!");
        out.println("x:");
        double x = scanner.nextDouble();
        out.println("y:");
        double y = scanner.nextDouble();

        return new Point(x, y);
    }

    private String createHeader() {
        StringBetter strHeader = new StringBetter("\t--");
        for (MenuInd val : this.prev)
            strHeader.append(val.name()).append("/");

        return strHeader.append(this.menuOpts.name()).append("--\n").red().toString();
    }

    private void tableDefault(List<List<String>> valTab, List<String> colLabl){
        ArrayList<String> linLabl = new ArrayList<>();
        for(int i = 0; i < valTab.size(); i++ )
            linLabl.add(String.format("%dº", i + 1));

        Table<String> tab = new Table<>(valTab,linLabl,colLabl);
        out.println(tab);
    }

    private String menuOptionText(int i) {
        switch (this.options.get(i)) {
            case INITIAL:
                return "Menu Initial";
            case REGISTER:
                return  "Registar novo utilizador";
            case REGISTERCLIENT:
                return "Registar novo Client";
            case REGISTEROWNER:
                return  "Registar novo Proprietário";
            case LOGIN:
                return  "Login";
            case HISTORYCLIENT:
            case HISTORYOWNER:
                return "Histórico de alugueres";
            case CLOSEST:
                return  "Carro mais próximo das suas coordenadas";
            case CHEAPEST:
                return"Carro mais barato";
            case CHEAPESTNEAR:
                return "Carro mais barato dentro de uma distância";
            case SPECIFIC:
                return "Carro específico";
            case AUTONOMY:
                return  "Carro com uma autonomia desejada.";
            case ADDCAR:
                return  "Adicionar novo carro";
            case CAROVERVIEW:
                return "Várias operações sobre carros";
            case REVIEWRENTAL:
                return  "Aceitar/rejeitar o aluguer de um determinado cliente;";
            case TOP10CLIENTS:
                return "UMCarroJá Challenge";
            case DISTANCE:
                return "Organizado por distância";
            case NUSES:
                return "Organizado por número de Utilizações";
            case RENTALS:
                return "Alugar um carro";
            case PENDING:
                return "Avaliações pendentes";
            default:
                return "";
        }
    }

    private void pickChildMenus() {
        this.options.clear();
        switch (this.menuOpts) {
            case INITIAL:
                this.options.add(MenuInd.LOGIN);
                this.options.add(MenuInd.REGISTER);
                break;
            case REGISTER:
                this.options.add(MenuInd.REGISTERCLIENT);
                this.options.add(MenuInd.REGISTEROWNER);
                break;
            case CLIENT:
                this.options.add(MenuInd.HISTORYCLIENT);
                this.options.add(MenuInd.PENDING);
                this.options.add(MenuInd.RENTALS);
                this.options.add(MenuInd.TOP10CLIENTS);
                break;
            case TOP10CLIENTS:
                this.options.add(MenuInd.NUSES);
                this.options.add(MenuInd.DISTANCE);
                break;
            case RENTALS:
                this.options.add(MenuInd.CLOSEST);
                this.options.add(MenuInd.CHEAPEST);
                this.options.add(MenuInd.CHEAPESTNEAR);
                this.options.add(MenuInd.SPECIFIC);
                this.options.add(MenuInd.AUTONOMY);
                break;
            case OWNER:
                this.options.add(MenuInd.HISTORYOWNER);
                this.options.add(MenuInd.CAROVERVIEW);
                this.options.add(MenuInd.REVIEWRENTAL);
                this.options.add(MenuInd.ADDCAR);
                break;
            default:
                break;
        }
    }

    @Override
    public String toString() {
        StringBuilder s = new StringBuilder();
        s.append(CLEAR);
        s.append(this.createHeader()).append("\n\n");

        for (int i = 0; i < this.options.size(); i++)
            s.append(i + 1).append("- ").append(this.menuOptionText(i)).append("\n");
        return s.toString();
    }
}
