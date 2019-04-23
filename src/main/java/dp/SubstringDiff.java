package dp;

public class SubstringDiff {

	static int substringDiff(int k, String s1, String s2) {
		int F[][][] = new int[s2.length() + 1][s1.length() + 1][2];


		//F[][][0] - used mistmatches
		//F[][][1] - current length of subsequence
		for (int i = 0; i < s1.length() + 1; i++) {
			F[0][i][0] = 0;
		}

		for (int i = 0; i < s2.length() + 1; i++) {
			F[i][0][0] = 0;
		}

		int maxVal = 0;

		for (int i = 1; i < s2.length() + 1; i++) {
			char s2c = s2.charAt(i - 1);
			for (int j = 1; j < s1.length() + 1; j++) {
				char s1c = s1.charAt(j - 1);

				F[i][j][0] = F[i - 1][j - 1][0] + (s1c == s2c ? 0 : 1);
				F[i][j][1] = F[i - 1][j - 1][1] + 1;

				int currMistmatches = F[i][j][0];
				int currLen = F[i][j][1];

				while (currMistmatches > k) {
					//back to the begin of string
					if (s2.charAt(i - currLen) != s1.charAt(j - currLen)) {
						currMistmatches--;
					}
					//cut first symbol in substring
					currLen--;
				}

				F[i][j][0] = currMistmatches;
				F[i][j][1] = currLen;

				maxVal = Math.max(F[i][j][1], maxVal);
			}
		}

//		for(int i=0;i<s1.length() + 1;i++){
//			for(int j=0;j<s1.length() + 1;j++){
//				System.out.print(F[i][j][0] + "/" + F[i][j][1] +" ");
//			}
//			System.out.println();
//		}

		return maxVal;
	}

	static int substringDiff2(int k, String s1, String s2) {
		int n = s1.length();
		int m = s2.length();
		int[][] L = new int[n + 1][m + 1];
		int[][] K = new int[n + 1][m + 1];

		for (int i = 0; i < n; i++) L[0][i] = K[0][i] = 0;
		for (int i = 0; i < m; i++) L[i][0] = K[i][0] = 0;

		int res = 0;

		for (int i = 1; i <= n; i++) {
			char curr = s1.charAt(i - 1);

			for (int j = 1; j <= m; j++) {
				L[i][j] = L[i - 1][j - 1] + 1; // running length
				K[i][j] = K[i - 1][j - 1] + (curr == s2.charAt(j - 1) ? 0 : 1); // running mismatches

				// calculate the  subsequence length by removing the  mismatches greater than k
				while (K[i][j] > k) {
					if (s1.charAt(i - L[i][j]) != s2.charAt(j - L[i][j])) K[i][j]--;
					L[i][j]--;
				}

				res = Math.max(res, L[i][j]);
			}
		}

		return res;
	}

	public static void main(String[] args) {

		System.out.println(substringDiff(2, "tabriz", "torino"));
		System.out.println(substringDiff(2, "cka", "aba"));
		System.out.println(substringDiff(3, "helloworld", "yellomarin"));

		System.out.println(substringDiff(11, "gatezejttddpkmndtauvjcffiiafgzhkqgzliirdldbmqkdfpeadgjxcirgkmkcfxorthhpujbnenxansboejjrqfxoohuolsxgohukxmpzfukzvkduurvajrodlpxojzsihiqftrbkixbcxraqpiyadbkzqihmigunrzfzcgzfkeszcpkdotulkktfduekyqzkymqpeidhpyuhotynqaxknnsheiogrhobrajzkrekexvorlvlyhtgstjrtdgjzahvmjnprcumulnvftgoiyctjgtthleeunkgbemapsntmfdnuaydkrbyngbrbpsznfdftonfmbjahqrpgddbkokvdxfflzysneapnqvsqhilxabbjamkdhpktscxsczpoontmliozurkrvagnidnmcayiradtbacltouzacvseayrdzkkkqgbfrrnfydbnnxvctffgtfpbmfzsqjnclcnttztcvvpbmkovahvpdnjqghcafzcxhrxumhxxkdtyjqypljzsbidfpoydlumdrhokvmstydlmymldvimdduvzzcmiyxapbrrrndhjnhncpibdmiryjteyvcydxkthxcbgxshffuzevvfrcrpzaotcpbefqqppehgdlbfstralgzbtdfervuvejyvvocrabkiohjxjnnrshvyijyonzzioeakpbkgxybtlcbybgzvvvvhhkdfvpupxnlecqizvzhgigliotybprnnntqdsiquxvdxojripdlmzsyorhjandaqtjfptgebxbjmnokevncfxkkadrsqjvxuttokcabefxehmnhkcbgrdmnmmycflifrrkriggeplcfafpxsbfchbdzbvdgsbrcebgkgsdbdntfdbaltnsdzraafhobrsygkvetomeqvkrntyzeqimcaktnvfcehaeexqjnjfyvomfqlbdjxhhjojvytaovvprpfrdpgurzfhknsimnmhctbkzxfxjrzfjvjsigivmlxcgiginjitarxettzzcpceonufetlpdxupmpfmhzanufjxrkaapaaalaulebxizfjshbjsagmxmuesvecuoobuctngykkzsztzauquxxdgmjxuybkzvxsftzpqhmarlsbaeriaahlrcdgjadhbrizmnabcfadtnfdzobdhayhrxmdddycenimblnrlicasqhttekqyiafijoiykcmutzbjupsbqxbzeyqxbsshelvzieoiozylenrlaelpykdpvzhvpttmsyxsjbrfqchgrxcuvkgqinluzjrqlnzitvhlofjiznsxvbbhscsuoufodozsrmjecfdrkcslgmmrcletuvxcdfitpmgocjdcurrbfqpefxtndzkuuzxpaxfanxdxnteiapkzouvqiykxntmltdpmyzjveivnfuhzlrkseyhpbgrtcvnqmpqcgubjyourdrizixmoflmyzsskvfagtdopgthcdmmqhkmksxgeckagmjgauvz",
				"gltezejtfdehkmndtauvicffiiafozhkqmzlieedldbmqjdfpeadgjxcirbkmkcfxorthhpujbnekzansboejjrqfxoohuozsxgohukxmpzbukzvkduurvajrodlpxojzsvhiqftrbgixbcxmaqpiyadbkzqnhmigunrzfzugzfkeszcpbdotulketftuekfqzkymqpeidhpyuhotynqqxknnsheitgrhobrajzkrekexvorlvlyhtmstjrldhjzahvmjnprgtgulhvftgoiyctjgtthleeunkgbemapsntmfdnuaydkrbyigbrbpsknfdftonfmbtahqrpgddbkokvdxfflzysneapnqvsqhilnabbjamkdhpktsxxsczpoontmliozurkrvagnidnmcayiradtbacltouzacvseayrdzkkkqgbfrrnfydbnnxuctfegtfpbmfzsqjnclcnttftcvvpbmkovahvpdnjqghcafzcxhrgumhoxkityjqypljzsbidfcoynlumdrhokvmstydlmymldvimrduvzzcmiyxapbrrrndhjnhncpjbdmiryjteyvcydxkthxcbgxshffyzevvfrcrpzaotcpbefqqppehgdlbfstaalgzbtdfervuvehyvvocrabkioojxjnnrshvyijymnnzioeakpbkgxdbtlobybgzvpvvhhkdcvplpvnlecqizvzhgiglsotobprnnntqdsiquxvdxojripdlmzsyorhjandaqtjfptgegxbjmnokevncfxkladrsqjvxugtokcabeqxehmnhkcbgrdmnmmyfsmifrrkriggeplcfbfpxsbfcxbdzbvdgsbrcebmkpsdbdntfdbaltnsdzrfafhobrsygktetomeqvkrztyzeqimcaktnvfcehaeexqjnjfyvomfqsbdjxhhjojvytaovvirpfrdpgurzfhknsimnmhctbkixfcjrzfjvjsigilmoxcuiginjitarxettzzcpcesnufbtlpdxupmpfmhzqnufjxrkaapaaalaulebxizfjshbjsagmxmueshecuoobzctngykkzsftzatquxxdgmjxuybkzvxsftzpqhmarlsbayriaahlrcdgjadhbrizmnaycfaftnfdzobdhhyhrxvddsycenimblnrcicasqhttekqyiafijoiykcmstzbjupsbqxbzeyqxbsshelvzxekiozylenrlaelpykdpvzhvpttmsyxsjbrfqchgrxcubkgqinluzjrqltzitvhlofjizxsxvbbqbcsuoufodozsrmjecfdricsljmmrcletuvxcxfitpmgocjdcurzbfqpefxtndzkuuzxpaxfanxdxntuiapkzoufqiykxntmlyepmyzjneyvnfuqzlrkseyhpbgrtcpnqmpqcgubjuourdriziimoflmyzsskvfagldopgthcdmmqhrmksxgeckarmjgauvj"));

	}

}

