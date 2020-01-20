//package model;

import java.security.NoSuchAlgorithmException;
import java.security.SecureRandom;
import java.time.LocalDateTime;
import java.util.Random;

class Weather {

    private Random a = SecureRandom.getInstanceStrong();

    private static final String WINTER = "Winter";
    private static final String SPRING = "Spring";
    private static final String SUMMER = "Summer";
    private static final String FALL = "Fall";

    private final String[] seasons = {
            WINTER, WINTER,
            SPRING, SPRING, SPRING,
            SUMMER, SUMMER, SUMMER,
            FALL, FALL, FALL,
            WINTER
    };

    Weather() throws NoSuchAlgorithmException {
    }

    private String getSeason() {
        return seasons[LocalDateTime.now().getMonthValue() - 1];
    }

    public double getSeasonDelay() {
        switch (getSeason()){
            case SUMMER:
                return this.a.nextDouble() % 0.1;

            case SPRING:
                return this.a.nextDouble() % 0.3;

            case FALL:
                return this.a.nextDouble() % 0.35;

            default:
                return this.a.nextDouble() % 0.6;
        }
    }
}
