using System.Collections;
using System.Collections.Generic;
using UnityEditor;
using UnityEngine;

public class CameraTrun : MonoBehaviour
{
	[SerializeField]
	private GameObject pivot;
	[SerializeField]
	private float turnSpeed = 5f;
    [SerializeField]
    private float cameraOffset = 5f;
	private float currentAngle = 0f;
	// Update is called once per frame
	void LateUpdate()
	{
        transform.localEulerAngles = new Vector3(45, currentAngle += (turnSpeed/100), 0);
		transform.position = pivot.transform.position - transform.forward * (cameraOffset * 10);
    }
}