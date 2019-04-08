package localcache;

public interface LocalCache {

	String get(String key);

	void put(String key, String value);

	void invalidate(String key);
}
