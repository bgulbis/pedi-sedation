SELECT DISTINCT
	ENCOUNTER.ENCNTR_ID AS ENCOUNTER_ID,
	ENCNTR_ALIAS.ALIAS AS FIN,
    ENCOUNTER.DISCH_DT_TM - ENCOUNTER.REG_DT_TM AS LOS,
    TRUNC(((pi_from_gmt(ENCOUNTER.REG_DT_TM, (pi_time_zone(1, @Variable('BOUSER'))))) - PERSON.BIRTH_DT_TM), 0) AS AGE_DAYS,
    TRUNC(((pi_from_gmt(ENCOUNTER.REG_DT_TM, (pi_time_zone(1, @Variable('BOUSER'))))) - PERSON.BIRTH_DT_TM) / 365.25, 0) AS AGE_YEARS,
    CV_SEX.DISPLAY AS SEX
FROM
	CODE_VALUE CV_SEX,
	ENCNTR_ALIAS,
	ENCOUNTER,
    PERSON,
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
        	AND (
        		CLINICAL_EVENT.EVENT_END_DT_TM + 0
        			BETWEEN DECODE(
        				@Prompt('Choose date range', 'A', {'Today', 'Yesterday', 'Week to Date', 'Last Week', 'Last Month', 'Month to Date', 'User-defined', 'N Days Prior'}, mono, free, , , User:79),
        				'Today', pi_to_gmt(TRUNC(SYSDATE), pi_time_zone(2, @Variable('BOUSER'))),
        				'Yesterday', pi_to_gmt(TRUNC(SYSDATE) - 1, pi_time_zone(2, @Variable('BOUSER'))),
        				'Week to Date', pi_to_gmt(TRUNC(SYSDATE, 'DAY'), pi_time_zone(2, @Variable('BOUSER'))),
        				'Last Week', pi_to_gmt(TRUNC(SYSDATE - 7, 'DAY'), pi_time_zone(2, @Variable('BOUSER'))),
        				'Last Month', pi_to_gmt(TRUNC(ADD_MONTHS(SYSDATE, -1), 'MONTH'), pi_time_zone(2, @Variable('BOUSER'))),
        				'Month to Date', pi_to_gmt(TRUNC(SYSDATE - 1, 'MONTH'), pi_time_zone(2, @Variable('BOUSER'))),
        				'User-defined', pi_to_gmt(
        					TO_DATE(
        						@Prompt('Enter begin date (Leave as 01/01/1800 if using a Relative Date)', 'D', , mono, free, persistent, {'01/01/1800 00:00:00'}, User:80),
        						pi_get_dm_info_char_gen('Date Format Mask|FT','PI EXP|Systems Configuration|Date Format Mask')
        					),
        					pi_time_zone(1, @Variable('BOUSER'))),
        				'N Days Prior', pi_to_gmt(SYSDATE - @Prompt('Days Prior to Now', 'N', , mono, free, persistent, {'0'}, User:2080), pi_time_zone(2, @Variable('BOUSER')))
        			)
        			AND DECODE(
        				@Prompt('Choose date range', 'A', {'Today', 'Yesterday', 'Week to Date', 'Last Week', 'Last Month', 'Month to Date', 'User-defined', 'N Days Prior'}, mono, free, , , User:79),
        				'Today', pi_to_gmt(TRUNC(SYSDATE) + (86399 / 86400), pi_time_zone(2, @Variable('BOUSER'))),
        				'Yesterday', pi_to_gmt(TRUNC(SYSDATE) - (1 / 86400), pi_time_zone(2, @Variable('BOUSER'))),
        				'Week to Date', pi_to_gmt(TRUNC(SYSDATE) - (1 / 86400), pi_time_zone(2, @Variable('BOUSER'))),
        				'Last Week', pi_to_gmt(TRUNC(SYSDATE, 'DAY') - (1 / 86400), pi_time_zone(2, @Variable('BOUSER'))),
        				'Last Month', pi_to_gmt(TRUNC(SYSDATE, 'MONTH') - (1 / 86400), pi_time_zone(2, @Variable('BOUSER'))),
        				'Month to Date', pi_to_gmt(TRUNC(SYSDATE) - (1 / 86400), pi_time_zone(2, @Variable('BOUSER'))),
        				'User-defined', pi_to_gmt(
        					TO_DATE(
        						@Prompt('Enter end date (Leave as 01/01/1800 if using a Relative Date)', 'D', , mono, free, persistent, {'01/01/1800 23:59:59'}, User:81),
        						pi_get_dm_info_char_gen('Date Format Mask|FT','PI EXP|Systems Configuration|Date Format Mask')
        					),
        					pi_time_zone(1, @Variable('BOUSER'))),
        				'N Days Prior', pi_to_gmt(SYSDATE, pi_time_zone(2, @Variable('BOUSER')))
        			)
        		AND CLINICAL_EVENT.EVENT_END_DT_TM
        			BETWEEN DECODE(
        				@Prompt('Choose date range', 'A', {'Today', 'Yesterday', 'Week to Date', 'Last Week', 'Last Month', 'Month to Date', 'User-defined', 'N Days Prior'}, mono, free, , , User:79),
        				'Today', TRUNC(SYSDATE),
        				'Yesterday', TRUNC(SYSDATE) - 1,
        				'Week to Date', TRUNC(SYSDATE, 'DAY'),
        				'Last Week', TRUNC(SYSDATE - 7, 'DAY'),
        				'Last Month', TRUNC(ADD_MONTHS(SYSDATE, -1), 'MONTH'),
        				'Month to Date', TRUNC(SYSDATE - 1, 'MONTH'),
        				'User-defined', DECODE(
        					@Prompt('Enter begin date (Leave as 01/01/1800 if using a Relative Date)', 'D', , mono, free, persistent, {'01/01/1800 00:00:00'}, User:80),
        					'01/01/1800 00:00:00',
        					'',
        					@Variable('Enter begin date (Leave as 01/01/1800 if using a Relative Date)')
        				),
        				'N Days Prior', SYSDATE - @Prompt('Days Prior to Now', 'N', , mono, free, persistent, {0}, User:2080)
        			) - 1
        			AND DECODE(
        				@Prompt('Choose date range', 'A', {'Today', 'Yesterday', 'Week to Date', 'Last Week', 'Last Month', 'Month to Date', 'User-defined', 'N Days Prior'}, mono, free, , , User:79),
        				'Today', TRUNC(SYSDATE) + (86399 / 86400),
        				'Yesterday', TRUNC(SYSDATE) - (1 / 86400),
        				'Week to Date', TRUNC(SYSDATE) - (1 / 86400),
        				'Last Week', TRUNC(SYSDATE, 'DAY') - (1 / 86400),
        				'Last Month', TRUNC(SYSDATE, 'MONTH') - (1 / 86400),
        				'Month to Date', TRUNC(SYSDATE) - (1 / 86400),
        				'User-defined', DECODE(
        					@Prompt('Enter end date (Leave as 01/01/1800 if using a Relative Date)', 'D', , mono, free, persistent, {'01/01/1800 23:59:59'}, User:81),
        					'01/01/1800 00:00:00',
        					'',
        					@Variable('Enter end date (Leave as 01/01/1800 if using a Relative Date)')
        				),
        				'N Days Prior', SYSDATE
        			) + 1
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
        	AND (
        		CLINICAL_EVENT.EVENT_END_DT_TM + 0
        			BETWEEN DECODE(
        				@Prompt('Choose date range', 'A', {'Today', 'Yesterday', 'Week to Date', 'Last Week', 'Last Month', 'Month to Date', 'User-defined', 'N Days Prior'}, mono, free, , , User:79),
        				'Today', pi_to_gmt(TRUNC(SYSDATE), pi_time_zone(2, @Variable('BOUSER'))),
        				'Yesterday', pi_to_gmt(TRUNC(SYSDATE) - 1, pi_time_zone(2, @Variable('BOUSER'))),
        				'Week to Date', pi_to_gmt(TRUNC(SYSDATE, 'DAY'), pi_time_zone(2, @Variable('BOUSER'))),
        				'Last Week', pi_to_gmt(TRUNC(SYSDATE - 7, 'DAY'), pi_time_zone(2, @Variable('BOUSER'))),
        				'Last Month', pi_to_gmt(TRUNC(ADD_MONTHS(SYSDATE, -1), 'MONTH'), pi_time_zone(2, @Variable('BOUSER'))),
        				'Month to Date', pi_to_gmt(TRUNC(SYSDATE - 1, 'MONTH'), pi_time_zone(2, @Variable('BOUSER'))),
        				'User-defined', pi_to_gmt(
        					TO_DATE(
        						@Prompt('Enter begin date (Leave as 01/01/1800 if using a Relative Date)', 'D', , mono, free, persistent, {'01/01/1800 00:00:00'}, User:80),
        						pi_get_dm_info_char_gen('Date Format Mask|FT','PI EXP|Systems Configuration|Date Format Mask')
        					),
        					pi_time_zone(1, @Variable('BOUSER'))),
        				'N Days Prior', pi_to_gmt(SYSDATE - @Prompt('Days Prior to Now', 'N', , mono, free, persistent, {'0'}, User:2080), pi_time_zone(2, @Variable('BOUSER')))
        			)
        			AND DECODE(
        				@Prompt('Choose date range', 'A', {'Today', 'Yesterday', 'Week to Date', 'Last Week', 'Last Month', 'Month to Date', 'User-defined', 'N Days Prior'}, mono, free, , , User:79),
        				'Today', pi_to_gmt(TRUNC(SYSDATE) + (86399 / 86400), pi_time_zone(2, @Variable('BOUSER'))),
        				'Yesterday', pi_to_gmt(TRUNC(SYSDATE) - (1 / 86400), pi_time_zone(2, @Variable('BOUSER'))),
        				'Week to Date', pi_to_gmt(TRUNC(SYSDATE) - (1 / 86400), pi_time_zone(2, @Variable('BOUSER'))),
        				'Last Week', pi_to_gmt(TRUNC(SYSDATE, 'DAY') - (1 / 86400), pi_time_zone(2, @Variable('BOUSER'))),
        				'Last Month', pi_to_gmt(TRUNC(SYSDATE, 'MONTH') - (1 / 86400), pi_time_zone(2, @Variable('BOUSER'))),
        				'Month to Date', pi_to_gmt(TRUNC(SYSDATE) - (1 / 86400), pi_time_zone(2, @Variable('BOUSER'))),
        				'User-defined', pi_to_gmt(
        					TO_DATE(
        						@Prompt('Enter end date (Leave as 01/01/1800 if using a Relative Date)', 'D', , mono, free, persistent, {'01/01/1800 23:59:59'}, User:81),
        						pi_get_dm_info_char_gen('Date Format Mask|FT','PI EXP|Systems Configuration|Date Format Mask')
        					),
        					pi_time_zone(1, @Variable('BOUSER'))),
        				'N Days Prior', pi_to_gmt(SYSDATE, pi_time_zone(2, @Variable('BOUSER')))
        			)
        		AND CLINICAL_EVENT.EVENT_END_DT_TM
        			BETWEEN DECODE(
        				@Prompt('Choose date range', 'A', {'Today', 'Yesterday', 'Week to Date', 'Last Week', 'Last Month', 'Month to Date', 'User-defined', 'N Days Prior'}, mono, free, , , User:79),
        				'Today', TRUNC(SYSDATE),
        				'Yesterday', TRUNC(SYSDATE) - 1,
        				'Week to Date', TRUNC(SYSDATE, 'DAY'),
        				'Last Week', TRUNC(SYSDATE - 7, 'DAY'),
        				'Last Month', TRUNC(ADD_MONTHS(SYSDATE, -1), 'MONTH'),
        				'Month to Date', TRUNC(SYSDATE - 1, 'MONTH'),
        				'User-defined', DECODE(
        					@Prompt('Enter begin date (Leave as 01/01/1800 if using a Relative Date)', 'D', , mono, free, persistent, {'01/01/1800 00:00:00'}, User:80),
        					'01/01/1800 00:00:00',
        					'',
        					@Variable('Enter begin date (Leave as 01/01/1800 if using a Relative Date)')
        				),
        				'N Days Prior', SYSDATE - @Prompt('Days Prior to Now', 'N', , mono, free, persistent, {0}, User:2080)
        			) - 1
        			AND DECODE(
        				@Prompt('Choose date range', 'A', {'Today', 'Yesterday', 'Week to Date', 'Last Week', 'Last Month', 'Month to Date', 'User-defined', 'N Days Prior'}, mono, free, , , User:79),
        				'Today', TRUNC(SYSDATE) + (86399 / 86400),
        				'Yesterday', TRUNC(SYSDATE) - (1 / 86400),
        				'Week to Date', TRUNC(SYSDATE) - (1 / 86400),
        				'Last Week', TRUNC(SYSDATE, 'DAY') - (1 / 86400),
        				'Last Month', TRUNC(SYSDATE, 'MONTH') - (1 / 86400),
        				'Month to Date', TRUNC(SYSDATE) - (1 / 86400),
        				'User-defined', DECODE(
        					@Prompt('Enter end date (Leave as 01/01/1800 if using a Relative Date)', 'D', , mono, free, persistent, {'01/01/1800 23:59:59'}, User:81),
        					'01/01/1800 00:00:00',
        					'',
        					@Variable('Enter end date (Leave as 01/01/1800 if using a Relative Date)')
        				),
        				'N Days Prior', SYSDATE
        			) + 1
        	)
	) ENCNTRS_DEXMED
WHERE
    ENCNTRS_DEXMED.ENCOUNTER_ID = ENCOUNTER.ENCNTR_ID
    AND ENCOUNTER.ACTIVE_IND = 1
	AND (
		ENCOUNTER.ENCNTR_ID = ENCNTR_ALIAS.ENCNTR_ID
		AND ENCNTR_ALIAS.ACTIVE_IND = 1
		AND ENCNTR_ALIAS.END_EFFECTIVE_DT_TM > SYSDATE
		AND ENCNTR_ALIAS.ENCNTR_ALIAS_TYPE_CD = 619
	)
    AND (
        ENCOUNTER.PERSON_ID = PERSON.PERSON_ID
        AND PERSON.ACTIVE_IND = 1
        AND PERSON.SEX_CD = CV_SEX.CODE_VALUE
    )
