package localcache;

import java.util.HashMap;
import java.util.Map;


/**
 * LRU local cache implementation.
 * Used double linked list to keep order of element insert operations.
 *
 * @author akorol
 */
public class LRULocalCache implements LocalCache {

	private int maxItems;

	private Map<String, Node> cache = new HashMap<>();

	//Double linked list for storing values insertion order
	private Node tail;
	private Node head;

	public LRULocalCache(int maxItems) {
		this.maxItems = maxItems;
	}

	public synchronized String get(String key) {
		Node val = cache.get(key);
		if (val != null) {
			if(val != head) {
				if(val == tail){
					tail = val.prev;
				} else {
					removeNode(val);
				}
				setHeader(val);
			}
			return val.value;
		} else {
			return null;
		}
	}

	private void setHeader(Node val) {
		if(head != null) {
			head.prev = val;
			val.next = head;
		}
		head = val;
		val.prev = null;
	}

	private void removeNode(Node val) {
		val.prev.next = val.next;
		if(val.next != null) {
			val.next.prev = val.prev;
		} else {
			val.next.prev = null;
		}
	}


	@Override
	public synchronized void invalidate(String key) {
		Node node = cache.get(key);
		if(node == null) {
			throw new IllegalArgumentException("No such key in cache");
		} else {
			if(node == head) {
				if(node.next != null) {
					setHeader(node.next);
				} else {
					head = null;
				}
			} else if(node == tail) {
				Node node1 = removeTail();
			} else {
				removeNode(node);
			}
			cache.remove(key);
		}
	}

	public synchronized void put(String key, String value) {
		if (cache.size() >= maxItems) {
			Node removedNode = removeTail();
			cache.remove(removedNode.key);
		}

		Node newNode = new Node(key, value, null, head);
		setHeader(newNode);
		if(tail == null) {
			tail = newNode;
		}
		cache.put(key, newNode);
	}

	private Node removeTail() {
		Node toReturn = tail;
		tail = tail.prev;
		tail.next = null;
		return toReturn;
	}

	public static class Node {

		private String value;
		private String key;

		private Node prev;
		private Node next;

		public Node(String key, String value, Node prev, Node next) {
			this.key = key;
			this.value = value;
			this.prev = prev;
			this.next = next;
		}

	}

}
