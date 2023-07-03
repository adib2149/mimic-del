-- NEEDS TO HAVE VENTSETTINGS, VENTDURATIONS FROM PREREQUISITE IN THIS FOLDER 
-- DELIRIUM_DATA.HAD_SURGERY: NULL means 0

DROP TABLE IF EXISTS delirium_patients CASCADE;
DROP TABLE IF EXISTS pneumonia_icd_codes CASCADE;
DROP TABLE IF EXISTS delirium_patients_diagnoses CASCADE;
DROP TABLE IF EXISTS delirium_patients_icd_with_pneu CASCADE;
DROP TABLE IF EXISTS delirium_prescriptions CASCADE;
DROP TABLE IF EXISTS delirium_drug CASCADE;
DROP TABLE IF EXISTS delirium_surgery CASCADE;
DROP TABLE IF EXISTS delirium_data CASCADE;

CREATE TABLE delirium_patients AS
SELECT subject_id, hadm_id FROM mimiciii.diagnoses_icd WHERE icd9_code = '2930' ORDER BY subject_id, hadm_id;

CREATE TABLE pneumonia_icd_codes AS
SELECT icd9_code, short_title from mimiciii.d_icd_diagnoses WHERE UPPER(long_title) LIKE '%PNEUMONIA%';

CREATE TABLE delirium_patients_diagnoses AS 
SELECT * FROM mimiciii.diagnoses_icd 
WHERE hadm_id IN (select hadm_id from delirium_patients) 
ORDER BY subject_id, seq_num ASC;

CREATE TABLE delirium_patients_icd_with_pneu AS 
SELECT l.subject_id, l.hadm_id, array_to_string(array_agg(r.icd9_str), E'\n') as icd9_codes, 
MAX(CASE WHEN l.icd9_code IN (SELECT icd9_code FROM pneumonia_icd_codes) THEN 1 ELSE 0 END) as had_pneu 
FROM delirium_patients_diagnoses l 
LEFT JOIN (
	SELECT icd9_code, icd9_code ||': '||short_title as icd9_str FROM mimiciii.d_icd_diagnoses
) r ON l.icd9_code = r.icd9_code
GROUP BY subject_id, hadm_id;

CREATE TABLE delirium_prescriptions AS 
SELECT l.*, keys.name as catagory FROM mimiciii.prescriptions l
INNER JOIN (VALUES
('ARIPIPRAZOLE'),
('ASENAPINE'),
('CARBAMAZEPINE'),
('CHLORPROMAZINE'),
('CLOZAPINE'),
('FLUPHENAZINE'),
('HALOPERIDOL'),
('ILOPERIDONE'),
('LITHIUM'),
('LOXAPINE'),
('LURASIDONE'),
('OLANZAPINE'),
('PALIPERIDONE'),
('PERPHENAZINE'),
('PROCHLORPERAZINE'),
('QUETIAPINE'),
('RISPERIDONE'),
('THIORIDAZINE'),
('THIOTHIXENE'),
('TRIFLUOPERAZINE'),
('ZIPRASIDONE'),
('AMISULPRIDE'),
('BLONANSERIN'),
('SULPIRIDE'),
('ZOTEPINE'),
('CLOTIAPINE'),
('DROPERIDOL'),
('MOSAPRAMINE'),
('MESORIDAZINE'),
('MOLINDONE'),
('FLUPENTHIXOL'),
('PIMOZIDE'),
('PEROSPIRONE'),
('PERPHENADAZINE'),
('TIAPRIDE'),
('ZUCLOPENTHIXOL')
) AS keys (name)
ON UPPER(l.drug) LIKE '%' || keys.name ||'%'
WHERE l.hadm_id IN (select hadm_id from delirium_patients);

CREATE TABLE delirium_drug AS 
SELECT subject_id, hadm_id, count(distinct catagory) as dist_cat_count, 
array_to_string(array_agg(drug), E'\n') as drugs, 
array_to_string(array_agg(distinct catagory), E'\n') as drug_catagories 
FROM (SELECT * FROM delirium_prescriptions) alias
GROUP BY subject_id, hadm_id;

CREATE TABLE delirium_surgery AS 
SELECT subject_id, hadm_id, MAX(CASE WHEN UPPER(sectionheader) LIKE '%SURGERY%' THEN 1 ELSE 0 END) as had_surgery 
FROM mimiciii.cptevents 
WHERE hadm_id IN (select hadm_id from delirium_patients) 
GROUP BY subject_id, hadm_id;

CREATE TABLE delirium_data AS 
SELECT ad.subject_id, patients.gender, 
	ROUND((cast(ad.admittime as date) - cast(patients.dob as date)) / 365.242, 2) as age, 
	ad.ethnicity, ad.hadm_id,
	icu.icustay_id, icu.los as los_days, icd.icd9_codes, 
	(SELECT sofa FROM mimiciii.sofa WHERE sofa.subject_id = ad.subject_id AND sofa.hadm_id = ad.hadm_id AND sofa.icustay_id = icu.icustay_id) sofa,
	(SELECT apsiii FROM mimiciii.apsiii WHERE apsiii.subject_id = ad.subject_id AND apsiii.hadm_id = ad.hadm_id AND apsiii.icustay_id = icu.icustay_id) apsiii,
 	(SELECT starttime FROM mimiciii.ventdurations WHERE ventdurations.icustay_id = icu.icustay_id AND ventdurations.ventnum = 1) mechvent_starttime,
	COALESCE((SELECT MAX(ventnum) FROM mimiciii.ventdurations WHERE ventdurations.icustay_id = icu.icustay_id), 0) mechvent,
	COALESCE((SELECT ARRAY_TO_STRING(ARRAY_AGG(duration_hours), ', ') FROM mimiciii.ventdurations WHERE ventdurations.icustay_id = icu.icustay_id), '') mechvent_duration_hours, 
	(CASE WHEN drg.drugs IS NOT NULL THEN drg.drugs ELSE 'No drug' END) AS drugs, 
	(CASE WHEN drg.dist_cat_count IS NOT NULL THEN drg.dist_cat_count ELSE 0 END) as dist_cat_count, 
	(CASE WHEN drg.drug_catagories IS NOT NULL THEN drg.drug_catagories ELSE 'NO DRUG' END) AS drug_categories,
	surg.had_surgery, 
	icd.had_pneu, 
	(CASE WHEN UPPER(icd.icd9_codes) LIKE '%99591:%' OR UPPER(icd.icd9_codes) LIKE '%99592:%' THEN 1 ELSE 0 END) as sepsis, 
	(CASE WHEN UPPER(icd.icd9_codes) LIKE '%29410:%' OR UPPER(icd.icd9_codes) LIKE '%29420:%' THEN 1 ELSE 0 END) as dementia, 
	(CASE WHEN UPPER(icd.icd9_codes) LIKE '%3310:%' THEN 1 ELSE 0 END) as alzheimers, 
	(CASE WHEN UPPER(icd.icd9_codes) LIKE '%311:%' OR UPPER(icd.icd9_codes) LIKE '%3004:%' THEN 1 ELSE 0 END) as depression, 
	(CASE WHEN UPPER(icd.icd9_codes) LIKE '%30000:%' THEN 1 ELSE 0 END) as anxiety, 
	(CASE WHEN (ROUND((cast(ad.deathtime as date) - cast(ad.dischtime as date)) / 365.242, 2) = 0) THEN 1 ELSE 0 END) as death_hosp, 
	(CASE WHEN (ROUND((cast(patients.dod as date) - cast(ad.dischtime as date)) / 365.242, 2) < 1) THEN 1 ELSE 0 END) as death_one_year, 
	(SELECT intime FROM mimiciii.icustays WHERE icustays.icustay_id = icu.icustay_id) drugstarttime,
	(SELECT admittime FROM mimiciii.admissions WHERE admissions.hadm_id = ad.hadm_id) admittime,
	(SELECT dischtime FROM mimiciii.admissions WHERE admissions.hadm_id = ad.hadm_id) dischtime,
	(SELECT deathtime FROM mimiciii.admissions WHERE admissions.hadm_id = ad.hadm_id) deathtime,
	(SELECT dod FROM mimiciii.patients WHERE patients.subject_id = ad.subject_id) patient_deathtime 
FROM mimiciii.admissions ad 
INNER JOIN mimiciii.patients patients 
ON patients.subject_id = ad.subject_id 
LEFT JOIN mimiciii.icustays icu 
ON ad.subject_id = icu.subject_id AND ad.hadm_id = icu.hadm_id 
INNER JOIN delirium_patients_icd_with_pneu icd 
ON ad.subject_id = icd.subject_id AND ad.hadm_id = icd.hadm_id 
LEFT JOIN delirium_drug drg 
ON ad.subject_id = drg.subject_id AND ad.hadm_id = drg.hadm_id 
LEFT JOIN delirium_surgery surg 
ON ad.subject_id = surg.subject_id AND ad.hadm_id = surg.hadm_id 
ORDER BY ad.subject_id;
