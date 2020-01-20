//package model;

//import utils.Point;

import java.util.ArrayList;
import java.util.List;

public class Client extends User {
    private static final long serialVersionUID = 6476239603804875239L;
    private Point pos;
    private final List<Rental> pendingRates;

    public Client(Point pos, String email, String passwd, String name, String address, int nif) {
        super(email, name, address, nif, passwd);
        this.pos = pos;
        this.pendingRates = new ArrayList<>();
    }

    public Client(Client clone) {
        super(clone);
        this.pos = clone.pos;
        this.pendingRates = new ArrayList<>(clone.pendingRates);
    }

    Point getPos() {
        return new Point(this.pos);
    }

    void addPendingRental(Rental r) {
        this.pendingRates.add(r);
    }

    public void rate(Rental r, int ratingCar, int ratingOwner) {
        r.rate(ratingCar, ratingOwner);
        this.pendingRates.remove(r);
    }

    void setPos(Point pos) {
        this.pos = pos;
    }

    public List<Rental> getPendingRates() {
        return new ArrayList<>(this.pendingRates);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;

        if (o == null || this.getClass() != o.getClass()) return false;

        Client client = (Client) o;
        return this.pos.equals(client.pos)
                && this.pendingRates.equals(client.pendingRates);
    }

    @Override
    public int hashCode() {
        return super.hashCode();
    }
}
