package stacks;

import java.io.IOException;
import java.util.Deque;
import java.util.LinkedList;
import java.util.Scanner;

import org.junit.Assert;

public class BalancedBrackets {

	// Complete the isBalanced function below.
	static String isBalanced(String s) {


		//To store order of open/close brackets will use stack structure
		Deque<Character> st = new LinkedList<>();

		for (char c : s.toCharArray()) {
			switch (c) {
				case '{':
				case '[':
				case '(':
					st.push(c);
					break;
				case '}':
					if (st.isEmpty() || st.peek() != '{') {
						return "NO";
					}
					st.poll();
					break;
				case ']':
					if (st.isEmpty() || st.peek() != '[') {
						return "NO";
					}
					st.poll();
					break;
				case ')':
					if (st.isEmpty() || st.peek() != '(') {
						return "NO";
					}
					st.poll();
			}
		}
		return st.isEmpty() ? "YES" : "NO";

	}

	public static void main(String[] args) throws IOException {
		Assert.assertEquals(isBalanced("{[(])}"), "NO");
		Assert.assertEquals(isBalanced("{{[[(())]]}}"), "YES");
	}

}
