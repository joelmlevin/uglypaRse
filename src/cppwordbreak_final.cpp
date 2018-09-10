#include <iostream>
#include <string>
#include <vector>
#include <Rcpp.h>
using namespace std;

int dictionaryContainsD(string &word, vector<string> MyDictionary)
{
	
	int nItems = MyDictionary.size();
	for (int i = 0; i < nItems; i++)
		if (MyDictionary[i].compare(word) == 0)
			return true;
	return false;
}

//wordBreakUtil
void wordBreakUtilD(string str, int size, string result, vector<string> MyDictionary, vector<string> &PossibleMatches);

// Prints all possible word breaks of given string
// [[Rcpp::export]]
vector<string> wordBreakD(string str, vector<string> MyDictionary)
{
	vector<string> PossibleMatches;
	//string FinalResult = "";
	// last argument is prefix
	wordBreakUtilD(str, str.size(), "", MyDictionary, PossibleMatches);
	return PossibleMatches;
}

// result store the current prefix with spaces between words
void wordBreakUtilD(string str, int n, string result, vector<string> MyDictionary, vector<string> &PossibleMatches)
{
	//Process all prefixes one by one
	for (int i = 1; i <= n; i++)
	{
		//extract substring from 0 to i in prefix
		string prefix = str.substr(0, i);

		// if dictionary conatins this prefix, then
		// we check for remaining string. Otherwise
		// we ignore this prefix (there is no else for
		// this if) and try next
		if (dictionaryContainsD(prefix, MyDictionary))
		{
			// if no more elements are there, print it
			if (i == n)
			{
				// add this element to previous prefix
				result += prefix;
				PossibleMatches.push_back(result);
				cout << result << endl; //print result
				return;
			}
			//FinalResult = result + prefix + " ";
			wordBreakUtilD(str.substr(i, n - i), n - i,
				result + prefix + " ", MyDictionary, PossibleMatches);
		}
	}	 //end for
	return;
}//end function


#ifdef TRASH
 //Un-Comment this block for independent testing
int main()
{	

	vector<string> dictionary2 = {"i", "dont" };
	vector<string> PossibleMatches2 = wordBreakD("ilove", dictionary2);
	for (int i = 0; i < PossibleMatches2.size(); i++)
	{
		cout << "D final result:" << PossibleMatches2[i] << endl;
	}

	return 0;
}
#endif



