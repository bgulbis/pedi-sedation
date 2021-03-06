SELECT DISTINCT
	ENCOUNTER.ENCNTR_ID AS ENCOUNTER_ID,
	TO_CHAR(pi_from_gmt(CLINICAL_EVENT.EVENT_END_DT_TM, (pi_time_zone(1, @Variable('BOUSER')))), 'YYYY-MM-DD"T"HH24:MI:SS') AS CLINICAL_EVENT_DATETIME,
	CV_EVENT.DISPLAY AS EVENT,
	CLINICAL_EVENT.RESULT_VAL AS RESULT,
	CV_RESULT_UNITS.DISPLAY AS RESULT_UNIT
FROM
    CLINICAL_EVENT,
    CODE_VALUE CV_EVENT,
    CODE_VALUE CV_RESULT_UNITS,
	ENCNTR_LOC_HIST,
	ENCOUNTER,
	(
        SELECT DISTINCT
            ENCOUNTER.ENCNTR_ID AS ENCOUNTER_ID
        FROM
            CLINICAL_EVENT,
        	ENCNTR_LOC_HIST,
            ENCOUNTER
        WHERE 
        	CLINICAL_EVENT.EVENT_CD = 37556709
        	AND CLINICAL_EVENT.VALID_UNTIL_DT_TM > DATE '2099-12-31' 
        	AND (
        		CLINICAL_EVENT.ENCNTR_ID = ENCOUNTER.ENCNTR_ID
        		AND CLINICAL_EVENT.PERSON_ID = ENCOUNTER.PERSON_ID
        		AND ENCOUNTER.ACTIVE_IND = 1
        		AND ENCOUNTER.LOC_FACILITY_CD = 3796
            )
            AND (
        		CLINICAL_EVENT.ENCNTR_ID = ENCNTR_LOC_HIST.ENCNTR_ID
        		AND ENCNTR_LOC_HIST.ACTIVE_IND = 1
        		AND CLINICAL_EVENT.EVENT_END_DT_TM >= ENCNTR_LOC_HIST.TRANSACTION_DT_TM
        		AND (CLINICAL_EVENT.EVENT_END_DT_TM BETWEEN ENCNTR_LOC_HIST.BEG_EFFECTIVE_DT_TM AND ENCNTR_LOC_HIST.END_EFFECTIVE_DT_TM)
        		AND ENCNTR_LOC_HIST.TRANSACTION_DT_TM = (
        			SELECT MAX(ELH.TRANSACTION_DT_TM)
        			FROM ENCNTR_LOC_HIST ELH
        			WHERE
        				CLINICAL_EVENT.ENCNTR_ID = ELH.ENCNTR_ID
        				AND ELH.ACTIVE_IND = 1
        				AND CLINICAL_EVENT.EVENT_END_DT_TM >= ELH.TRANSACTION_DT_TM
        		)
        		AND ENCNTR_LOC_HIST.LOC_NURSE_UNIT_CD = 1101851
        	)
        	AND CLINICAL_EVENT.EVENT_END_DT_TM BETWEEN
				pi_to_gmt(
					TO_DATE(
						@Prompt('Enter begin date', 'D', , mono, free, persistent, {'01/01/1800 00:00:00'}, User:0), 
						pi_get_dm_info_char_gen('Date Format Mask|FT','PI EXP|Systems Configuration|Date Format Mask')
					), 
					'America/Chicago'
				)
				AND pi_to_gmt(
					TO_DATE(
						@Prompt('Enter end date', 'D', , mono, free, persistent, {'01/01/1800 00:00:00'}, User:1), 
						pi_get_dm_info_char_gen('Date Format Mask|FT','PI EXP|Systems Configuration|Date Format Mask')
					) - 1/86400, 
					'America/Chicago'
				)
        
        MINUS
        
        SELECT DISTINCT
            ENCOUNTER.ENCNTR_ID AS ENCOUNTER_ID
        FROM
            CLINICAL_EVENT,
        	ENCNTR_LOC_HIST,
            ENCOUNTER
        WHERE 
        	CLINICAL_EVENT.EVENT_CD = 37556577
        	AND CLINICAL_EVENT.VALID_UNTIL_DT_TM > DATE '2099-12-31' 
        	AND (
        		CLINICAL_EVENT.ENCNTR_ID = ENCOUNTER.ENCNTR_ID
        		AND CLINICAL_EVENT.PERSON_ID = ENCOUNTER.PERSON_ID
        		AND ENCOUNTER.ACTIVE_IND = 1
        		AND ENCOUNTER.LOC_FACILITY_CD = 3796
            )
            AND (
        		CLINICAL_EVENT.ENCNTR_ID = ENCNTR_LOC_HIST.ENCNTR_ID
        		AND ENCNTR_LOC_HIST.ACTIVE_IND = 1
        		AND CLINICAL_EVENT.EVENT_END_DT_TM >= ENCNTR_LOC_HIST.TRANSACTION_DT_TM
        		AND (CLINICAL_EVENT.EVENT_END_DT_TM BETWEEN ENCNTR_LOC_HIST.BEG_EFFECTIVE_DT_TM AND ENCNTR_LOC_HIST.END_EFFECTIVE_DT_TM)
        		AND ENCNTR_LOC_HIST.TRANSACTION_DT_TM = (
        			SELECT MAX(ELH.TRANSACTION_DT_TM)
        			FROM ENCNTR_LOC_HIST ELH
        			WHERE
        				CLINICAL_EVENT.ENCNTR_ID = ELH.ENCNTR_ID
        				AND ELH.ACTIVE_IND = 1
        				AND CLINICAL_EVENT.EVENT_END_DT_TM >= ELH.TRANSACTION_DT_TM
        		)
        		AND ENCNTR_LOC_HIST.LOC_NURSE_UNIT_CD = 1101851
        	)
        	AND CLINICAL_EVENT.EVENT_END_DT_TM BETWEEN
				pi_to_gmt(
					TO_DATE(
						@Prompt('Enter begin date', 'D', , mono, free, persistent, {'01/01/1800 00:00:00'}, User:0), 
						pi_get_dm_info_char_gen('Date Format Mask|FT','PI EXP|Systems Configuration|Date Format Mask')
					), 
					'America/Chicago'
				)
				AND pi_to_gmt(
					TO_DATE(
						@Prompt('Enter end date', 'D', , mono, free, persistent, {'01/01/1800 00:00:00'}, User:1), 
						pi_get_dm_info_char_gen('Date Format Mask|FT','PI EXP|Systems Configuration|Date Format Mask')
					) - 1/86400, 
					'America/Chicago'
				)
	) ENCNTRS_DEXMED
WHERE
    ENCNTRS_DEXMED.ENCOUNTER_ID = ENCOUNTER.ENCNTR_ID
    AND ENCOUNTER.ACTIVE_IND = 1
    AND (
        ENCOUNTER.ENCNTR_ID = CLINICAL_EVENT.ENCNTR_ID
        AND CLINICAL_EVENT.EVENT_CD IN (30107, 30066, 102497)
    	AND CLINICAL_EVENT.VALID_UNTIL_DT_TM > DATE '2099-12-31' 
        AND CLINICAL_EVENT.EVENT_CD = CV_EVENT.CODE_VALUE
        AND CLINICAL_EVENT.RESULT_UNITS_CD = CV_RESULT_UNITS.CODE_VALUE
    )
	AND (
		CLINICAL_EVENT.ENCNTR_ID = ENCNTR_LOC_HIST.ENCNTR_ID
		AND ENCNTR_LOC_HIST.ACTIVE_IND = 1
		AND CLINICAL_EVENT.EVENT_END_DT_TM >= ENCNTR_LOC_HIST.TRANSACTION_DT_TM
		AND (CLINICAL_EVENT.EVENT_END_DT_TM BETWEEN ENCNTR_LOC_HIST.BEG_EFFECTIVE_DT_TM AND ENCNTR_LOC_HIST.END_EFFECTIVE_DT_TM)
		AND ENCNTR_LOC_HIST.TRANSACTION_DT_TM = (
			SELECT MAX(ELH.TRANSACTION_DT_TM)
			FROM ENCNTR_LOC_HIST ELH
			WHERE
				CLINICAL_EVENT.ENCNTR_ID = ELH.ENCNTR_ID
				AND ELH.ACTIVE_IND = 1
				AND CLINICAL_EVENT.EVENT_END_DT_TM >= ELH.TRANSACTION_DT_TM
		)
		AND ENCNTR_LOC_HIST.LOC_NURSE_UNIT_CD = 1101851
	)
