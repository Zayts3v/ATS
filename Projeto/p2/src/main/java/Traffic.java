//package model;

import java.security.NoSuchAlgorithmException;
import java.security.SecureRandom;
import java.time.LocalDateTime;
import java.util.Random;

class Traffic {

    private Random b = SecureRandom.getInstanceStrong();

    Traffic() throws NoSuchAlgorithmException {
    }

    public double getTraficDelay(double delay) {
        int a = LocalDateTime.now().getHour();
        if(a == 18 || a == 8)
            return (this.b.nextDouble() % 0.6) + (delay % 0.2);
        if(a > 1 && a < 6)
            return (this.b.nextDouble() % 0.1) + (delay % 0.2);
        return (this.b.nextDouble() % 0.3) + (delay % 0.2);
    }

}
