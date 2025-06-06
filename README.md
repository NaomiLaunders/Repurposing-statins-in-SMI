# Psychiatric symptom improvement from adjunctive statin prescribing in severe mental illness: three target trial emulation studies

We conducted three target trial emulations using CPRD, a database of primary care data in the UK. Our hypotheses were:

1: As simvastatin has the most potential to cross the BBB, based on its lipophilic nature and molecular weight, patients with SMI initiating simvastatin in combination with antipsychotic or mood stabilisers will have lower psychiatric hospital admissions than those initiating atorvastatin, pravastatin or rosuvastatin.

2: As simvastatin and atorvastatin inhibit p-glycoprotein and aripiprazole, risperidone and olanzapine have affinity for this:

                                    A: patients initiating these statins in combination with these antipsychotics will have lower psychiatric hospital admissions than those initiating pravastatin (no p-glycoprotein inhibition) and the same antipsychotics.
                                      
                                    B: patients with ongoing prescriptions of these statins initiating these antipsychotics will have lower psychiatric hospital admissions than those prescribed these statins initiating quetiapine (not a substrate of p-glycoprotein).

Through testing these hypotheses we aimed to determine the potential mechanism of action of statins in improving psychiatric symptoms in people with SMI. We use the target trial emulation framework, a causal inference approach to analysing observational data in line with a hypothetical trial, with an aim of improving study design and limiting bias.

Read our pre-print here:
[Psychiatric symptom improvement from adjunctive statin prescribing in severe mental illness: three target trial emulation studies](https://www.medrxiv.org/content/10.1101/2025.01.20.25320829v1)

## Understanding the files

Scripts are organised into chunks: Code lists (scripts beginning 1), observations (scripts beginning 2), cohort creation (scripts beginning 3) and analysis (scripts beginning 4).
Only a subset of scripts have been provided for chunks 1-3, but all analysis files have been uploaded.
Code lists were generated in CPRD. They are specific to CPRD and to the exact database builds. When re-using codelists it's advisable to search for new terms in the system being used.
CPRD code lists use medcodes. These long numeric identifiers must be kept as character vectors in R, Stata and Excel to avoid truncation.

This project uses pseudonymised patient records and so no data is available for uploading.
