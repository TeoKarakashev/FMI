import bg.sofia.uni.fmi.mjt.splitwise.command.executors.CommandExecutor;
import bg.sofia.uni.fmi.mjt.splitwise.command.executors.CommandExecutorAPI;
import bg.sofia.uni.fmi.mjt.splitwise.server.Server;
import bg.sofia.uni.fmi.mjt.splitwise.storage.InMemoryStorage;
import bg.sofia.uni.fmi.mjt.splitwise.storage.Storage;

public class Engine {
    private static final int SERVER_PORT = 8080;

    public static void main(String[] args) {

        Storage storage = new InMemoryStorage();

        CommandExecutor commandExecutor = new CommandExecutorAPI(storage);

        Server server = new Server(SERVER_PORT, commandExecutor);

        server.start();
        server.stop();
    }
}