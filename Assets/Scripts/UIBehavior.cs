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
	private List<List<int>> case1, case2, case3;
	// Start is called before the first frame update
	void Start(){
		for(int i = 1; i<82; i++){
			listInput.Add(canva.transform.GetChild(i).gameObject);
		}
		case1 = new List<List<int>>(){
			new List<int>() {0, 6, 0, 1, 0, 4, 0, 5, 0}, //0
            new List<int>() {0, 0, 8, 3, 0, 5, 6, 0, 0}, //1
			new List<int>() {2, 0, 0, 0, 0, 0, 0, 0 ,1}, //2
			new List<int>() {8, 0, 0, 4, 0, 7, 0, 0, 6}, //3
			new List<int>() {0, 0, 6, 0, 0, 0, 3, 0, 0}, //4
			new List<int>() {7, 0, 0, 9, 0, 1, 0, 0, 4}, //5
			new List<int>() {5, 0, 0, 0, 0, 0, 0, 0, 2}, //6
			new List<int>() {0, 0, 7, 2, 0, 6, 9, 0, 0}, //7
			new List<int>() {0, 4, 0, 5, 0, 8, 0, 7, 0}  //8
        };

		case2 = new List<List<int>>()
		{
            new List<int>() {0, 0, 0, 0, 0, 4, 9, 0, 0}, //0
            new List<int>() {0, 0, 5, 3, 2, 0, 0, 0, 0}, //1
			new List<int>() {2, 0, 0, 0, 0, 6, 0, 4, 0}, //2
			new List<int>() {8, 0, 4, 0, 0, 0, 0, 6, 0}, //3
			new List<int>() {0, 5, 0, 0, 6, 0, 0, 1, 0}, //4
			new List<int>() {0, 1, 0, 0, 0, 0, 3, 0, 9}, //5
			new List<int>() {0, 2, 0, 8, 0, 0, 0, 0, 6}, //6
			new List<int>() {0, 0, 0, 0, 7, 9, 1, 0, 0}, //7
			new List<int>() {0, 0, 9, 5, 0, 0, 0, 0, 0}  //8
		};

        case3 = new List<List<int>>()
        {
            new List<int>() {0, 0, 9, 0, 2, 8, 0, 0, 0}, //0
            new List<int>() {0, 8, 0, 0, 0, 0, 9, 0, 0}, //1
			new List<int>() {0, 7, 0, 0, 5, 0, 0, 0, 0}, //2
			new List<int>() {0, 3, 8, 9, 0, 0, 1, 0, 5}, //3
			new List<int>() {0, 0, 0, 0, 0, 0, 0, 0, 0}, //4
			new List<int>() {6, 0, 4, 0, 0, 5, 2, 9, 0}, //5
			new List<int>() {0, 0, 0, 0, 4, 0, 0, 6, 0}, //6
			new List<int>() {0, 0, 6, 0, 0, 0, 0, 3, 0}, //7
			new List<int>() {0, 0, 0, 7, 3, 0, 5, 0, 0}  //8
		};
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
			solution.GetComponent<TextMeshProUGUI>().text = "Nodes: " + result.Value.nodes + "\nABF: " + result.Value.branching.ToString("0.00") + "\nTime: "+ result.Value.time.Minutes + ":" + result.Value.time.Seconds + "." + result.Value.time.Milliseconds + " s";
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
	private void placeTestCase(int o){
		for(int i = 0; i < listInput.Count; i++){
			int num = o == 0 ? case1[i / 9][i % 9] : o == 1 ? case2[i / 9][i % 9] : case3[i / 9][i % 9];
            listInput[i].GetComponentInChildren<TMP_InputField>().text = num == 0 ? "" : num.ToString();
        }
	}

	public void placeTestCaseOne(){
		placeTestCase(0);

    }
	public void placeTestCaseTwo(){
        placeTestCase(1);
    }

	public void placeTestCaseThree(){
        placeTestCase(2);
    }

	public void QuitGame(){
		#if UNITY_EDITOR
			UnityEditor.EditorApplication.isPlaying = false;
		#else
			Application.Quit();
		#endif
    }
}