using System.Collections;
using System.Collections.Generic;
using System.Runtime.InteropServices;
using UnityEngine;
using Search;
using System.Runtime.CompilerServices;
using Microsoft.FSharp.Collections;
using System.Linq;
using TMPro;

public class UIBehavior : MonoBehaviour{
	private List<List<int>> auxiliarList;
	[SerializeField]
	private GameObject canva;
	private List<GameObject> listInput = new List<GameObject>();	
	// Start is called before the first frame update
	void Start(){
		for(int i = 0; i<81; i++){
			listInput.Add(canva.transform.GetChild(i).gameObject);
		}
	}
	// Update is called once per frame
	void Update(){
		if (Input.GetKeyDown(KeyCode.Space)){
			ObtainInput();
            FSharpList<FSharpList<int>> myFSharpList = ListModule.OfSeq(
				auxiliarList.Select(l => ListModule.OfSeq(l))
			);
            var result = Search.solution.output(myFSharpList);
			if (result != null)
			{
                Debug.Log(result.ToString());
            }else
			{
                Debug.Log("There is no solution for that input :(");
            }
			
        }
	}
	void ObtainInput(){
		List<int> values = new List<int>();
		foreach (GameObject i in listInput){
			string aux = i.GetComponentInChildren<TMP_InputField>().text;
			if (aux != null && aux != ""){
                values.Add(int.Parse(aux));
			}else{
                values.Add(0);
            }
		}
		auxiliarList = new List<List<int>>();
		for (int i = 0; i < 9; i++){
			auxiliarList.Add(values.GetRange(i*9, 9));
        }
    }
}