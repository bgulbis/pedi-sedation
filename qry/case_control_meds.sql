WITH PATIENTS AS (
	SELECT DISTINCT
		ENCOUNTER.ENCNTR_ID,
		ENCOUNTER.PERSON_ID
	FROM
		ENCOUNTER
	WHERE
	    ENCOUNTER.ENCNTR_ID IN @prompt('Encounter ID','A',,Multi,Free,Persistent,,User:0)
), DOSES AS (
	SELECT DISTINCT
		PATIENTS.ENCNTR_ID,
		PATIENTS.PERSON_ID,
		CLINICAL_EVENT.EVENT_END_DT_TM,
		TO_CHAR(pi_from_gmt(CLINICAL_EVENT.EVENT_END_DT_TM, 'America/Chicago'), 'YYYY-MM-DD"T"HH24:MI:SS') AS MED_DATETIME,
		CLINICAL_EVENT.EVENT_ID,
		pi_get_cv_display(CLINICAL_EVENT.EVENT_CD) AS MEDICATION,
		CE_MED_RESULT.ADMIN_DOSAGE AS DOSE,
		pi_get_cv_display(CE_MED_RESULT.DOSAGE_UNIT_CD) AS DOSE_UNIT,
		CE_MED_RESULT.INFUSION_RATE AS RATE,
		pi_get_cv_display(CE_MED_RESULT.INFUSION_UNIT_CD) AS RATE_UNIT,
		ORDER_DETAIL.OE_FIELD_DISPLAY_VALUE AS DOSE_WEIGHT,
		pi_get_cv_display(CE_MED_RESULT.IV_EVENT_CD) AS IV_EVENT,
		pi_get_cv_display(CE_MED_RESULT.ADMIN_ROUTE_CD) AS ROUTE
	FROM
		CE_MED_RESULT,
		CLINICAL_EVENT,
		ORDER_DETAIL,
		-- ORDER_DETAIL OD_WT_UNITS,
		PATIENTS
	WHERE
		PATIENTS.ENCNTR_ID = CLINICAL_EVENT.ENCNTR_ID
		AND CLINICAL_EVENT.EVENT_CLASS_CD = 158 -- MED
		AND PATIENTS.PERSON_ID = CLINICAL_EVENT.PERSON_ID
		AND CLINICAL_EVENT.EVENT_CD IN (
			37556577, -- clonidine
			37556956, -- FENTanyl
			37557204, -- HYDROmorphone
			37557589, -- midazolam
			37557455, -- LORAzepam
			37557538 -- methadone
		)
		AND CLINICAL_EVENT.VALID_UNTIL_DT_TM > DATE '2099-12-31' 
		AND CLINICAL_EVENT.EVENT_ID = CE_MED_RESULT.EVENT_ID
		AND CE_MED_RESULT.VALID_UNTIL_DT_TM > DATE '2099-12-31'
		AND CLINICAL_EVENT.ORDER_ID = ORDER_DETAIL.ORDER_ID(+)
		AND ORDER_DETAIL.OE_FIELD_MEANING_ID(+) = 99 -- WEIGHT
		AND ORDER_DETAIL.ACTION_SEQUENCE(+) = 1
		-- AND CLINICAL_EVENT.ORDER_ID = OD_WT_UNITS.ORDER_ID(+)
		-- AND OD_WT_UNITS.OE_FIELD_MEANING_ID(+) = 100 -- WEIGHTUNIT
		-- AND OD_WT_UNITS.ACTION_SEQUENCE(+) = 1
)

SELECT DISTINCT
	DOSES.*,
	MAX(CLINICAL_EVENT.RESUlT_VAL) KEEP (DENSE_RANK LAST ORDER BY DOSES.EVENT_END_DT_TM, DOSES.EVENT_ID) OVER (PARTITION BY DOSES.ENCNTR_ID) AS WEIGHT	
FROM
	CLINICAL_EVENT,
	DOSES
WHERE
	DOSES.ENCNTR_ID = CLINICAL_EVENT.ENCNTR_ID(+)
	AND CLINICAL_EVENT.EVENT_CLASS_CD(+) = 159 -- NUM
	AND DOSES.PERSON_ID = CLINICAL_EVENT.PERSON_ID(+)
	AND CLINICAL_EVENT.EVENT_CD(+) = 30107 -- Weight
	AND CLINICAL_EVENT.EVENT_END_DT_TM(+) <= DOSES.EVENT_END_DT_TM
	AND CLINICAL_EVENT.VALID_UNTIL_DT_TM(+) > DATE '2099-12-31'	
	AND CLINICAL_EVENT.RESULT_UNITS_CD(+) = 170 -- kg
