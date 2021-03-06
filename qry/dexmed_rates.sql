WITH DEXM_PTS AS (
	SELECT DISTINCT
		CLINICAL_EVENT.ENCNTR_ID,
		CLINICAL_EVENT.PERSON_ID
	FROM
		CLINICAL_EVENT,
		ENCNTR_LOC_HIST
	WHERE
		CLINICAL_EVENT.EVENT_CD = 37556709 -- dexmedetomidine
		AND CLINICAL_EVENT.EVENT_END_DT_TM BETWEEN
			pi_to_gmt(
				TO_DATE(
					@Prompt('Enter begin date', 'D', , mono, free, persistent, {'06/01/2018 00:00:00'}, User:0), 
					pi_get_dm_info_char_gen('Date Format Mask|FT','PI EXP|Systems Configuration|Date Format Mask')
				), 
				'America/Chicago'
			)
			AND pi_to_gmt(
				TO_DATE(
					@Prompt('Enter end date', 'D', , mono, free, persistent, {'07/01/2019 00:00:00'}, User:1), 
					pi_get_dm_info_char_gen('Date Format Mask|FT','PI EXP|Systems Configuration|Date Format Mask')
				) - 1/86400, 
				'America/Chicago'
			)
		AND CLINICAL_EVENT.ENCNTR_ID = ENCNTR_LOC_HIST.ENCNTR_ID
		AND ENCNTR_LOC_HIST.BEG_EFFECTIVE_DT_TM <= CLINICAL_EVENT.EVENT_END_DT_TM
		AND ENCNTR_LOC_HIST.TRANSACTION_DT_TM = (
			SELECT MAX(ELH.TRANSACTION_DT_TM)
			FROM ENCNTR_LOC_HIST ELH
			WHERE
				CLINICAL_EVENT.ENCNTR_ID = ELH.ENCNTR_ID
				AND ELH.TRANSACTION_DT_TM <= CLINICAL_EVENT.EVENT_END_DT_TM
		)
		AND ENCNTR_LOC_HIST.END_EFFECTIVE_DT_TM >= CLINICAL_EVENT.EVENT_END_DT_TM
		AND ENCNTR_LOC_HIST.LOC_NURSE_UNIT_CD = 1101851 -- HC PICU
), CLON_PTS AS (
	SELECT DISTINCT
		CLINICAL_EVENT.ENCNTR_ID,
		CLINICAL_EVENT.PERSON_ID
	FROM
		CLINICAL_EVENT,
		ENCNTR_LOC_HIST
	WHERE
		CLINICAL_EVENT.EVENT_CD = 37556577 -- clonidine
		AND CLINICAL_EVENT.EVENT_END_DT_TM BETWEEN
			pi_to_gmt(
				TO_DATE(
					@Prompt('Enter begin date', 'D', , mono, free, persistent, {'06/01/2018 00:00:00'}, User:0), 
					pi_get_dm_info_char_gen('Date Format Mask|FT','PI EXP|Systems Configuration|Date Format Mask')
				), 
				'America/Chicago'
			)
			AND pi_to_gmt(
				TO_DATE(
					@Prompt('Enter end date', 'D', , mono, free, persistent, {'07/01/2019 00:00:00'}, User:1), 
					pi_get_dm_info_char_gen('Date Format Mask|FT','PI EXP|Systems Configuration|Date Format Mask')
				) - 1/86400, 
				'America/Chicago'
			)
		AND CLINICAL_EVENT.ENCNTR_ID = ENCNTR_LOC_HIST.ENCNTR_ID
		AND ENCNTR_LOC_HIST.BEG_EFFECTIVE_DT_TM <= CLINICAL_EVENT.EVENT_END_DT_TM
		AND ENCNTR_LOC_HIST.TRANSACTION_DT_TM = (
			SELECT MAX(ELH.TRANSACTION_DT_TM)
			FROM ENCNTR_LOC_HIST ELH
			WHERE
				CLINICAL_EVENT.ENCNTR_ID = ELH.ENCNTR_ID
				AND ELH.TRANSACTION_DT_TM <= CLINICAL_EVENT.EVENT_END_DT_TM
		)
		AND ENCNTR_LOC_HIST.END_EFFECTIVE_DT_TM >= CLINICAL_EVENT.EVENT_END_DT_TM
		AND ENCNTR_LOC_HIST.LOC_NURSE_UNIT_CD = 1101851 -- HC PICU
), PATIENTS AS (
	SELECT * FROM DEXM_PTS
	
	MINUS
	
	SELECT * FROM CLON_PTS
)

SELECT DISTINCT
	PATIENTS.ENCNTR_ID AS ENCOUNTER_ID,
	-- pi_from_gmt(CLINICAL_EVENT.EVENT_END_DT_TM, (pi_time_zone(1, @Variable('BOUSER')))) AS EVENT_DATETIME,
	TO_CHAR(pi_from_gmt(CLINICAL_EVENT.EVENT_END_DT_TM, (pi_time_zone(1, @Variable('BOUSER')))), 'YYYY-MM-DD"T"HH24:MI:SS') AS MED_DATETIME,
	CLINICAL_EVENT.EVENT_ID,
	pi_get_cv_display(CLINICAL_EVENT.EVENT_CD) AS MEDICATION,
	CE_MED_RESULT.ADMIN_DOSAGE AS DOSE,
	pi_get_cv_display(CE_MED_RESULT.DOSAGE_UNIT_CD) AS DOSE_UNIT,
	CE_MED_RESULT.INFUSION_RATE AS RATE,
	pi_get_cv_display(CE_MED_RESULT.INFUSION_UNIT_CD) AS RATE_UNIT,
	pi_get_cv_display(CE_MED_RESULT.IV_EVENT_CD) AS IV_EVENT,
	pi_get_cv_display(CE_MED_RESULT.ADMIN_ROUTE_CD) AS ROUTE
	/* CLINICAL_EVENT.ORDER_ID AS ORDER_ID,
	ORDERS.TEMPLATE_ORDER_ID AS TEMPLATE_ORDER_ID,
	CASE 
		WHEN ORDERS.TEMPLATE_ORDER_ID = 0 THEN CLINICAL_EVENT.ORDER_ID
		ELSE ORDERS.TEMPLATE_ORDER_ID
	END AS ORIG_ORDER_ID */
FROM
	CE_MED_RESULT,
	CLINICAL_EVENT,
	ENCNTR_LOC_HIST,
	-- ORDERS,
	PATIENTS
WHERE
	PATIENTS.ENCNTR_ID = CLINICAL_EVENT.ENCNTR_ID
	AND CLINICAL_EVENT.EVENT_CLASS_CD = 158 -- MED
	AND PATIENTS.PERSON_ID = CLINICAL_EVENT.PERSON_ID
	AND CLINICAL_EVENT.EVENT_CD = 37556709 -- dexmedetomidine
	AND CLINICAL_EVENT.VALID_UNTIL_DT_TM > DATE '2099-12-31' 
	AND CLINICAL_EVENT.EVENT_ID = CE_MED_RESULT.EVENT_ID
	AND CE_MED_RESULT.VALID_UNTIL_DT_TM > DATE '2099-12-31'
	AND CLINICAL_EVENT.ENCNTR_ID = ENCNTR_LOC_HIST.ENCNTR_ID
	AND ENCNTR_LOC_HIST.BEG_EFFECTIVE_DT_TM <= CLINICAL_EVENT.EVENT_END_DT_TM
	AND ENCNTR_LOC_HIST.TRANSACTION_DT_TM = (
		SELECT MAX(ELH.TRANSACTION_DT_TM)
		FROM ENCNTR_LOC_HIST ELH
		WHERE
			CLINICAL_EVENT.ENCNTR_ID = ELH.ENCNTR_ID
			AND ELH.TRANSACTION_DT_TM <= CLINICAL_EVENT.EVENT_END_DT_TM
	)
	AND ENCNTR_LOC_HIST.END_EFFECTIVE_DT_TM >= CLINICAL_EVENT.EVENT_END_DT_TM
	-- AND CLINICAL_EVENT.ORDER_ID = ORDERS.ORDER_ID
