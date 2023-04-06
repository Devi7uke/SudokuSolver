using System.Collections;
using System.Collections.Generic;
using System.Runtime.InteropServices;
using UnityEngine;
using Search;
using System.Runtime.CompilerServices;
using Microsoft.FSharp.Collections;
using System.Linq;

public class UIBehavior : MonoBehaviour{
	[SerializeField]
	private FSharpList<FSharpList<int>> list;
	// Start is called before the first frame update
	void Start(){
		
	}

	// Update is called once per frame
	void Update(){
        List<List<int>> myList = new List<List<int>>(){
			new List<int>() {2, 5, 0, 0, 3, 0, 9, 0, 1},
			new List<int>() {0, 1, 0, 0, 0, 4, 0, 0, 0},
			new List<int>() {4, 0, 7, 0, 0, 0, 2, 0, 8},
			new List<int>() {0, 0, 5, 2, 0, 0, 0, 0, 0},
			new List<int>() {0, 0, 0, 0, 9, 8, 1, 0, 0},
			new List<int>() {0, 4, 0, 0, 0, 3, 0, 0, 0},
			new List<int>() {0, 0, 0, 3, 6, 0, 0, 7, 2},
			new List<int>() {0, 7, 0, 0, 0, 0, 0, 0, 3},
			new List<int>() {9, 0, 3, 0, 0, 0, 6, 0, 4}
		};
        FSharpList<FSharpList<int>> myFSharpList = ListModule.OfSeq(
            myList.Select(l => ListModule.OfSeq(l))
        );
        var result = Search.solution.output(myFSharpList);
		
		Debug.Log(result.ToString());
	}
}