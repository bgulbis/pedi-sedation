WITH DEXM_PTS AS (
	SELECT DISTINCT
		CLINICAL_EVENT.ENCNTR_ID
	FROM
		CLINICAL_EVENT,
		ENCNTR_LOC_HIST
	WHERE
		CLINICAL_EVENT.EVENT_CD = 37556709 -- dexmedetomidine
		AND CLINICAL_EVENT.EVENT_END_DT_TM BETWEEN
			pi_to_gmt(
				TO_DATE(
					@Prompt('Enter begin date', 'D', , mono, free, persistent, {'01/01/1800 00:00:00'}, User:0), 
					pi_get_dm_info_char_gen('Date Format Mask|FT','PI EXP|Systems Configuration|Date Format Mask')
				), 
				'CST'
			)
			AND pi_to_gmt(
				TO_DATE(
					@Prompt('Enter end date', 'D', , mono, free, persistent, {'01/01/1800 00:00:00'}, User:1), 
					pi_get_dm_info_char_gen('Date Format Mask|FT','PI EXP|Systems Configuration|Date Format Mask')
				) - 1/86400, 
				'CST'
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
		CLINICAL_EVENT.ENCNTR_ID
	FROM
		CLINICAL_EVENT,
		ENCNTR_LOC_HIST
	WHERE
		CLINICAL_EVENT.EVENT_CD = 37556577 -- clonidine
		AND CLINICAL_EVENT.EVENT_END_DT_TM BETWEEN
			pi_to_gmt(
				TO_DATE(
					@Prompt('Enter begin date', 'D', , mono, free, persistent, {'01/01/1800 00:00:00'}, User:0), 
					pi_get_dm_info_char_gen('Date Format Mask|FT','PI EXP|Systems Configuration|Date Format Mask')
				), 
				'CST'
			)
			AND pi_to_gmt(
				TO_DATE(
					@Prompt('Enter end date', 'D', , mono, free, persistent, {'01/01/1800 00:00:00'}, User:1), 
					pi_get_dm_info_char_gen('Date Format Mask|FT','PI EXP|Systems Configuration|Date Format Mask')
				) - 1/86400, 
				'CST'
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
	pi_from_gmt(ENCNTR_LOC_HIST.BEG_EFFECTIVE_DT_TM, (pi_time_zone(1, @Variable('BOUSER')))) AS BEGIN_EFFECTIVE_DATETIME,
	pi_from_gmt(ENCNTR_LOC_HIST.END_EFFECTIVE_DT_TM, (pi_time_zone(1, @Variable('BOUSER')))) AS END_EFFECTIVE_DATETIME,
	pi_from_gmt(ENCNTR_LOC_HIST.TRANSACTION_DT_TM, (pi_time_zone(1, @Variable('BOUSER')))) AS TRANSACTION_DATETIME,
	pi_get_cv_display(ENCNTR_LOC_HIST.LOC_NURSE_UNIT_CD) AS NURSE_UNIT
FROM
	ENCNTR_LOC_HIST,
	PATIENTS
WHERE
	PATIENTS.ENCNTR_ID = ENCNTR_LOC_HIST.ENCNTR_ID
