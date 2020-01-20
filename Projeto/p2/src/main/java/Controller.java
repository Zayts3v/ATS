//package controller;

//import exceptions.*;
//import model.*;
//import view.Menu;
//import view.viewmodel.*;

import java.security.NoSuchAlgorithmException;
import java.util.*;
import java.util.stream.Collectors;

public class Controller {
    private final UMCarroJa model;
    private User user;
    private final Menu menu;

    private String error;
    private static final String INVALID_PARAMS = "Parametros Inválidos";
    private static final String NO_AVAILABLE_CARS = "No cars availables";
    private static final String UNKNOWN_COMPARE = "Ai, ups!";

    public Controller(UMCarroJa model) {
        this.menu = new Menu();
        this.model = model;
    }

    public void login() {
        try {
            NewLogin r = menu.newLogin(error);
            user = model.logIn(r.getUser(), r.getPassword());
            menu.selectOption((user instanceof Client)? Menu.MenuInd.CLIENT : Menu.MenuInd.OWNER);
            this.error = "";
        }
        catch (InvalidUserException e){ this.error = "Invalid Username"; }
        catch (WrongPasswordExecption e){ this.error = "Invalid Password"; }
    }
    public void registerClient() {
        try {
            RegisterUser registerUserCli = menu.newRegisterUser(error);
            Client client = new Client(
                    registerUserCli.getPos(),
                    registerUserCli.getEmail(),
                    registerUserCli.getPasswd(),
                    registerUserCli.getName(),
                    registerUserCli.getAddress(),
                    registerUserCli.getNif()
            );
            this.model.addUser(client);
            menu.back();
            error = "";
        }
        catch (InvalidNewRegisterException e){ error = INVALID_PARAMS; }
        catch (UserExistsException e){ error = "Utilizador já existe"; }
    }
    public void registerOwner() {
        try {
            RegisterUser registerUserProp = menu.newRegisterUser(error);
            Owner owner = new Owner(
                    registerUserProp.getEmail(),
                    registerUserProp.getName(),
                    registerUserProp.getAddress(),
                    registerUserProp.getNif(),
                    registerUserProp.getPasswd()
            );
            this.model.addUser(owner);
            menu.back();
            error = "";
        }
        catch (InvalidNewRegisterException e){ error = INVALID_PARAMS; }
        catch (UserExistsException e){ error = "Utilizador já existe"; }
    }
    public void filter(String filter) {
        try{
            RentCarSimple rent = menu.simpleCarRent(error);
            Rental rental = model.rental(
                    (Client)user,
                    rent.getPoint(),
                    filter,
                    rent.getCarType());
            menu.showString(rental.toString());
            menu.back();
            error = "";
        }
        catch (UnknownCompareTypeException ignored) {this.error = UNKNOWN_COMPARE;}
        catch (NoCarAvaliableException e) { error = NO_AVAILABLE_CARS; }
        catch (InvalidNewRentalException e){error = "Novo Rental inválido"; }
    }

    public void reviewRental() {
        Owner owner = (Owner)this.user;
        List<Rental> lR = owner.getPending();
        if (lR.isEmpty()) {
            this.menu.back();
            return;
        }
        String v = menu.reviewRentShow(
                error,
                owner.getRates(),
                lR.stream()
                        .map(Rental::toParsableUserString)
                        .map(x -> Arrays.asList(x.split("\n")))
                        .collect(Collectors.toList()));

        try {
            char op = v.charAt(0);
            if(op == 'a') {
                this.model.rent(lR.get(Integer.parseInt(v.substring(1)) - 1));
                this.model.rate(
                        owner,
                        lR.get(Integer.parseInt(v.substring(1)) - 1),
                        this.menu.showRentalRate(
                                lR.get(Integer.parseInt(v.substring(1)) - 1).toFinalString()));
            }
            else if (op == 'r') {
                this.model.refuse(owner, lR.get(Integer.parseInt(v.substring(1)) - 1));
            }
            else if( op == 'b' ) {
                this.menu.back();
            }

            this.error = "";
        }
        catch(NumberFormatException | IndexOutOfBoundsException | NoSuchAlgorithmException e){ error = "Input Inválido"; }
    }
    public void cheapestNear() {
        try{
            CheapestNearCar walkCar = menu.walkingDistanceRent(error);

            Rental rental = model.rental(
                    (Client)user,
                    walkCar.getPoint(),
                    walkCar.getWalkDistance(),
                    walkCar.getType()
            );

            this.menu.showString(rental.toString());
            this.menu.back();
            error = "";
        }
        catch (InvalidNewRentalException e){error = "New rental inválido";}
        catch (NoCarAvaliableException e)  {error = NO_AVAILABLE_CARS; }
    }
    public void autonomy() {
        try{
            AutonomyCar autoCar = menu.autonomyCarRent(error);

            Rental rental = model.rental(
                    autoCar.getPoint(),
                    autoCar.getAutonomy(),
                    autoCar.getType(),
                    (Client)user);

            menu.showString(rental.toString());
            this.menu.back();
            error = "";
        }
        catch (InvalidNewRentalException e){error = "New rental inválido";}
        catch (NoCarAvaliableException e) { error = NO_AVAILABLE_CARS; }
    }
    public void specific() {
        try {
            SpecificCar sc = this.menu.specificCarRent(error);
            Rental rental = this.model.rental(sc.getPoint(), sc.getNumberPlate(), (Client)user);
            this.menu.showString(rental.toString());
            this.menu.back();
            error = "";
        }
        catch (NoCarAvaliableException e) { error = "Carro não está disponível"; }
        catch (InvalidCarException e) { error = "Carro não existe"; }
        catch (InvalidNewRentalException e) { error = INVALID_PARAMS; }
    }
    public void addCar() {
        try {
            RegisterCar registerCar = menu.newRegisterCar(error);
            Owner ownerCar = (Owner)this.user;
            model.addCar(
                    ownerCar,
                    registerCar.getNumberPlate(),
                    registerCar.getType(),
                    registerCar.getAvgSpeed(),
                    registerCar.getBasePrice(),
                    registerCar.getGasMileage(),
                    registerCar.getRange(),
                    registerCar.getPos(),
                    registerCar.getBrand()
            );
            menu.back();
            error = "";
        }
        catch (InvalidNewRegisterException e){ error = INVALID_PARAMS; }
        catch (CarExistsException e){ error = "Carro já existe"; }
        catch (InvalidUserException ignored) { this.error = "xau user!";}
    }

    public void nUses() {
        menu.top10ClientsShow(
                this.model.getBestClientsTimes()
                        .stream()
                        .map(x ->
                                Arrays.asList(
                                        x.getKey(),
                                        x.getValue().toString()))
                        .limit(10)
                        .collect(Collectors.toList()),
                "Número de Utilizações");
        this.menu.back();
    }

    public void distance() {
        menu.top10ClientsShow(
                this.model.getBestClientsTravel()
                        .stream()
                        .map(x ->
                                Arrays.asList(
                                        x.getKey(),
                                        String.format("%.2f", x.getValue())))
                        .limit(10)
                        .collect(Collectors.toList()),
                "Distância");
        this.menu.back();
    }

    public void carOverview() {
        Owner ownerCar = (Owner)this.user;
        String action = this.menu.carOverviewShow(error,
                ownerCar.getCars().stream()
                        .map(x -> Arrays.asList(x.toString().split("\n")))
                        .collect(Collectors.toList()));
        try {
            switch (action.charAt(0)) {
                case ' ':
                    break;
                case 'r':
                    model.refil(ownerCar, Integer.parseInt(action.substring(1)) - 1);
                    break;
                case'c':
                    String [] s = action.substring(1).split(" ");
                    if (s.length != 2)
                        throw new InvalidNumberOfArgumentsException();
                    model.setBasePrice(ownerCar, Integer.parseInt(s[0]) - 1, Double.parseDouble(s[1]));
                    break;
                case 'd':
                    model.swapState(ownerCar, Integer.parseInt(action.substring(1)) - 1);
                    break;
                case 't':
                    TimeInterval ti = this.menu.getTimeInterval(error);
                    this.menu.showString("Total faturado: " +
                            model.getTotalBilledCar(
                                    ownerCar.getCars().get(Integer.parseInt(action.substring(1)) - 1),
                                    ti.getInicio(),
                                    ti.getFim()));
                    break;
                case 'b':
                    this.menu.back();
                    break;

                default:
                    throw new InvalidNumberOfArgumentsException();
            }
            error = "";
        }
        catch (IndexOutOfBoundsException e){ error = "Valor de carro inválido"; }
        catch (NumberFormatException e){ error = "Posição inválida"; }
        catch (InvalidNumberOfArgumentsException e) {error = INVALID_PARAMS;}
        catch (InvalidTimeIntervalException e ){error = "Formato Inválido de Data";}
    }
    public void pending() {
        try {
            Client cli = (Client) user;
            List<Rental> pR = cli.getPendingRates();

            if (pR.isEmpty()) {
                this.menu.back();
                return;
            }

            RateOwnerCar r = this.menu.pendingRateShow(error, pR.get(0).toString(), pR.size());

            model.rate(cli, pR.get(0), r.getOwnerRate(), r.getCarRate());

            error = "";
        }
        catch (InvalidRatingException e){error = INVALID_PARAMS;}
    }
    public void historyOwner() {
        try{
            TimeInterval ti = this.menu.getTimeInterval(error);

            this.menu.rentalHistoryShow(ti,
                    this.model.getRentalListOwner((Owner) this.user, ti.getInicio(), ti.getFim())
                            .stream()
                            .map(Rental::toParsableOwnerRentalString)
                            .map(x -> Arrays.asList(x.split("\n")))
                            .collect(Collectors.toList()));

            this.menu.back();
            error = "";
        }
        catch (InvalidTimeIntervalException e){error = "Intervalo Inválido";}
    }
    public void historyClient() {
        try{
            TimeInterval ti = this.menu.getTimeInterval(error);

            this.menu.rentalHistoryShow(ti,
                    this.model.getRentalListClient((Client) this.user, ti.getInicio(), ti.getFim())
                            .stream()
                            .map(Rental::toParsableUserRentalString)
                            .map(x -> Arrays.asList(x.split("\n")))
                            .collect(Collectors.toList()));

            this.menu.back();
            error = "";
        }
        catch (InvalidTimeIntervalException e){error = "Intervalo Inválido";}
    }

    public void run() {
        while(this.menu.getRun()) {
            switch (menu.getMenuOpts()) {
                case LOGIN:
                    login();
                    break;
                case REGISTERCLIENT:
                    registerClient();
                    break;
                case REGISTEROWNER:
                    registerOwner();
                    break;
                case CLOSEST:
                    filter("MaisPerto");
                    break;
                case CHEAPEST:
                    filter("MaisBarato");
                    break;
                case REVIEWRENTAL:
                    reviewRental();
                    break;
                case CHEAPESTNEAR:
                    cheapestNear();
                    break;
                case AUTONOMY:
                    autonomy();
                    break;
                case SPECIFIC:
                    specific();
                    break;
                case ADDCAR:
                    addCar();
                    break;
                case NUSES:
                    nUses();
                    break;
                case DISTANCE:
                    distance();
                    break;
                case CAROVERVIEW:
                    carOverview();
                    break;
                case PENDING:
                    pending();
                    break;
                case HISTORYOWNER:
                    historyOwner();
                    break;
                case HISTORYCLIENT:
                    historyClient();
                    break;
                default:
                    this.menu.parser();
                    break;
            }
        }
    }
}
