package dp;

import java.lang.reflect.Array;
import java.util.Arrays;

public class CoinChangeProblem {


	static long getWays2(int n, int[] c) {
		int[][] A = new int[c.length][(int) (n + 1)];

		for (int i = 0; i < c.length; i++) {
			A[i][0] = 1;
		}
		for (int i = 0; i < c.length; i++) {
			for (int j = 1; j <= n; j++) {
				int x = (i > 0) ? A[i - 1][j] : 0;
				int y = (j - c[i] >= 0) ? A[i][j - c[i]] : 0;
				A[i][j] = x + y;
			}
		}

		for (int i = 0; i < c.length; i++) {
			for (int j = 0; j < n + 1; j++) {
				System.out.print(A[i][j] + " ");
			}
			System.out.println();
		}

		return A[c.length - 1][n];
	}

	static long getWays(long n, long[] c) {

		Arrays.sort(c);

		//Will use reccurent formula:
		//if w >= c[i] T[i, w] = T[i-1, w] + T[i, w-c[i]]
		// if w< c[i] T[i, w] = T[i-1, w]

		//Fill for w = 0 c={0}

		long[][] T = new long[c.length][(int) (n + 1)];

		for (int i = 0; i < c.length; i++) {
			T[i][0] = 1;
		}

		for (int coinIndex = 0; coinIndex < c.length; coinIndex++) {
			long currentCoin = c[coinIndex];
			for (int sum = 1; sum <= n; sum++) {

				long x = coinIndex > 0 ? T[coinIndex - 1][sum] : 0;
				long y = (sum - currentCoin) >= 0 ? T[coinIndex][(int) (sum - currentCoin)] : 0;

				T[coinIndex][sum] = x+y;
			}
		}

		for (int i = 0; i < c.length; i++) {
			for (int j = 0; j < n + 1; j++) {
				System.out.print(T[i][j] + " ");
			}
			System.out.println();
		}

		return T[c.length - 1][(int) (n)];

	}


	public static void main(String[] args) {
		System.out.println(getWays(10, new long[]{2, 5, 3, 6}));
	}

}
