//package view.viewmodel;

public class NewLogin {
    private final String user;
    private final String password;

    public NewLogin(String user, String password) {
        this.user = user;
        this.password = password;
    }

    public String getUser() {
        return user;
    }

    public String getPassword() {
        return password;
    }
}
