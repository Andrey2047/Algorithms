package throttle;

public interface Throttle {

	boolean isAllowed(Object request);
}
