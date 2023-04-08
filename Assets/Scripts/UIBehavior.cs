using System.Collections;
using System.Collections.Generic;
using System.Runtime.InteropServices;
using UnityEngine;
using Search;
using System.Runtime.CompilerServices;
using Microsoft.FSharp.Collections;
using System.Linq;
using TMPro;
using System;

public class UIBehavior : MonoBehaviour{
	private List<List<int>> auxiliarList;
	[SerializeField]
	private GameObject canva;
	[SerializeField]
	private GameObject statusText;
	[SerializeField]
	private GameObject solution;
	private List<GameObject> listInput = new List<GameObject>();
	// Start is called before the first frame update
	void Start(){
		for(int i = 1; i<82; i++){
			listInput.Add(canva.transform.GetChild(i).gameObject);
		}
	}
	private void ObtainInput(){
		List<int> values = new List<int>();
		foreach (GameObject i in listInput){
			string aux = i.GetComponentInChildren<TMP_InputField>().text;
			//string aux = i.ge
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

	private void ShowSolution(int e){
        statusText.GetComponent<TextMeshProUGUI>().text = "Status: Running";
        FSharpList<FSharpList<int>> myFSharpList = ListModule.OfSeq(auxiliarList.Select(l => ListModule.OfSeq(l)));
		var result = e==0 ? Search.solution.outputDFS(myFSharpList) : Search.solution.outputAStar(myFSharpList);
		if (result != null){
            var outputSolution = result.Value.grid;
            List<List<int>> outputList = ListModule.OfSeq(outputSolution).Select(innerList => ListModule.OfSeq(innerList).ToList()).ToList();
            for (int i = 0; i<listInput.Count; i++)
			{
				listInput[i].GetComponentInChildren<TMP_InputField>().text = "";
                listInput[i].GetComponentInChildren<TMP_InputField>().text = outputList[i/9][i%9].ToString();
            }
			solution.GetComponent<TextMeshProUGUI>().text = "Nodes: " + result.Value.nodes + "\nABF: " + result.Value.branching.ToString("0.00") + "\nTime: " + result.Value.time.Seconds + "." + result.Value.time.Milliseconds + " s";
            statusText.GetComponent<TextMeshProUGUI>().text = "Status: Solved";
        }
        else{
			Debug.Log("There is no solution for that input :(");
            statusText.GetComponent<TextMeshProUGUI>().text = "Status: No Solution";
        }
	}
	public void ShowAASSolution(){
        statusText.GetComponent<TextMeshProUGUI>().text = "Status: Running";
        ObtainInput();
        ShowSolution(1);
    }
    public void ShowDFSSolution(){
        ObtainInput();
        ShowSolution(0);
    }
    public void ResetGrid(){
        for (int i = 0; i < listInput.Count; i++){
            listInput[i].GetComponentInChildren<TMP_InputField>().text = "";
        }
        statusText.GetComponent<TextMeshProUGUI>().text = "Status: Waiting";
		solution.GetComponent<TextMeshProUGUI>().text = "Nodes: 0\nABF: 0.00\nTime: 00:0000";
    }
	public void QuitGame(){
		#if UNITY_EDITOR
			UnityEditor.EditorApplication.isPlaying = false;
		#else
			Application.Quit();
		#endif
    }
}