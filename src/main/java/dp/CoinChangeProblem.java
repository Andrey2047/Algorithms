package dp;

import java.util.Arrays;

public class CoinChangeProblem {

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

		return T[c.length - 1][(int) (n)];
	}


	public static void main(String[] args) {
		System.out.println(getWays(10, new long[]{2, 5, 3, 6}));
	}

}
