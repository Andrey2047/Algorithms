package localcache;

import org.junit.Test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

public class LRULocalCacheTest {

	@Test
	public void testInsertElement(){
		LRULocalCache localCache = new LRULocalCache(2);
		localCache.put("key1", "value1");
		assertEquals(localCache.get("key1"), "value1");
	}

	@Test
	public void testInsertElementWithFullCache(){
		LRULocalCache localCache = new LRULocalCache(2);
		localCache.put("key1", "value1");
		localCache.put("key2", "value1");
		localCache.put("key3", "value1");
		localCache.put("key4", "value1");
		assertNull(localCache.get("key1"));
		assertNull(localCache.get("key2"));
	}

	@Test
	public void testRemoveElementWithLRU(){
		LRULocalCache localCache = new LRULocalCache(3);
		localCache.put("key1", "value1");
		localCache.put("key2", "value2");
		localCache.put("key3", "value3");


		localCache.get("key3");
		localCache.get("key2");
		localCache.get("key1");

		localCache.put("key4", "value4");

		assertNull(localCache.get("key3"));
		assertNotNull(localCache.get("key1"));
	}

	@Test
	public void testInvalidateElement(){
		LRULocalCache localCache = new LRULocalCache(3);
		localCache.put("key1", "value1");
		localCache.put("key2", "value2");
		localCache.put("key3", "value3");
		localCache.invalidate("key1");

		assertNull(localCache.get("key1"));
	}

}