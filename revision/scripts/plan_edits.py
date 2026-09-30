"""Define minimal, traceable manuscript edits against exact source wording."""
import json
from pathlib import Path
from docx import Document
ROOT=Path(__file__).resolve().parents[1]
DOC=Document(ROOT/'docs/Manuscript.docx')
EDITS=[]

def change(p: int, old: str, new: str, refs: str, reason: str='Reviewer revision') -> None:
    """Require an exact unique source anchor before recording a revision."""
    assert DOC.paragraphs[p].text.count(old)==1,(p,old)
    EDITS.append(dict(paragraph=p,old=old,new=new,comments=refs,reason=reason))

def append(p: int, text: str, refs: str) -> None:
    EDITS.append(dict(paragraph=p,old='',new=' '+text,comments=refs,reason='Reviewer revision'))

change(5,'Restoration significantly enhanced:', 'Restored reefs significantly increased ecosystem processes above degraded baselines in:', '1, 10','Report the direction of the significant habitat-type comparisons')
change(5,'We trained and rigorously evaluated machine learning models to identify 34 biological sound types within this data','We trained and rigorously evaluated 34 project-specific machine learning classifiers to identify 15 biological sonotypes within these data','5','Correct confusion between 15 sonotypes and 34 classifiers')
change(5,'These detections were used to infer four key functions','These detections were used to infer four key ecosystem processes','1','Use the manuscript’s ecosystem-process terminology')
change(10,'sediment bioturbation, providing','sediment bioturbation, the mixing and reworking of sediment by organisms, providing','2')
change(12,'rubble stabilisation and coral out-planting methodology','rubble stabilisation and coral out-planting methodology, which secures loose coral rubble to provide a stable substrate for coral fragments (section 3.1)','3')
change(17,'We gathered basic ecological survey data from healthy and degraded sites to aid in their selection (Table S1).','Basic ecological surveys supported the selection of healthy and degraded sites, using coral cover in Australia, Indonesia, Kenya and Mexico, and fish biomass and species richness in the Maldives (Table S1).','14, 19','Adapt supported methodological clarification from thesis chapter 5')
change(17,'installed less than three months prior','installed up to three months prior','11a','Table S1 and the age CSV include sites at three months')
for p,old in [(5,'(<3 months)'),(12,'(<3 months)'),(63,'(< 3 months)')]:
    change(p,old,'(≤3 months)','11a','Align category boundary with Table S1')
append(17,'These categories describe the sampled age groups rather than established recovery stages; no sites between 3 and 32 months post-restoration were available.','11a')
change(20,'enabled in the AudioMoth configuration application settings (v1.4)','enabled using the AudioMoth configuration application (v1.4) to configure the HydroMoth recorders','4')
change(26,'(Fig. 2, 1c-6c)','(Fig. 2, 1a and 2c–6c)','18')
change(26,'1280-dimension embeddings of coral reef audio','1280-dimension embeddings of coral reef audio, numerical representations that summarise acoustic features for comparison and classification','5')
change(25,'Arrows indicate process flow; numbers denote the order of steps.','Arrows indicate process flow. Suffixes a, b and c distinguish the embedding, manual screening and unsupervised discovery branches before classifier training; step 1a supplies the raw audio to all three. Steps 13a and 13b distinguish final prediction evaluation from calculation of ecological indicators.','18')
change(34,'(Fig. 2, 3-6)','(Fig. 2, 3–8)','18')
change(35,'Here, I first trained','Here, we first trained','6','Grammar: use the manuscript’s collective author voice')
change(35,'used to inference over','used to run inference over','6','Grammar: inference is a noun')
# Figure labels checked directly against the supplied image.
change(36,'(Fig. 2, 10)','(Fig. 2, 9)','18')
# Two identical Fig.2,11 anchors: replace the complete corresponding clauses.
change(36,'direct within-bin score comparisons (Fig. 2, 11)','direct within-bin score comparisons (Fig. 2, 10)','18')
change(36,'evaluation performed again (Fig. 2, 11)','evaluation performed again following the decision at step 11 (Fig. 2)','18')
change(36,'rate of false positives that persist','proportion of retained detections that were false positives','16','Precision audit samples predicted positives; this is not the conventional false-positive rate')
change(36,'The mean false positive rate after this evaluation','The mean false-positive proportion among retained detections','16')
change(39,'The nightly count of putative fish sonotype detections (which excludes snapping shrimp) was then multiplied by the recorder’s duty cycle to provide a score representing the abundance of nighttime fish sounds.','The nightly count of putative fish sonotype detections (excluding snapping shrimp) was modelled with the log of recorded nighttime hours as an exposure offset. This measures acoustic detection rate rather than fish abundance; multiple detections may originate from the same individual.','7, 8','Align Methods with the actual cuescape code and CSV exposure')
append(39,'Phonic richness was not multiplied by duty cycle. To test sensitivity to recording effort, we also retained each of the two complementary four-minute recording phases in Indonesia, recalculated daily richness and refitted the model combining all projects (Supplementary 1, Text S1).','12')
change(40,'and multiplying this by the recorder’s duty cycle.','and modelling these raw counts with the log of recorded hours as an exposure offset.','7, 8')
change(40,'and multiplying by the recorder’s duty cycle.','and modelling these raw counts with the log of recorded hours as an exposure offset.','7, 8')
append(40,'The attribution of Scrape to parrotfish grazing has not been validated by direct observation in every study region. In Mexico, 26 of 100 checked Scrape detections were false positives (Table S2); this uncertainty may obscure habitat differences, and we cannot attribute all errors to a particular alternative sound source.','16')
change(44,'Each function exhibited strong left skewness with overdispersion at high values. To account for this, we fitted a generalised linear mixed model (GLMM) with a negative binomial error distribution to each function individually.','We fitted generalised linear mixed models (GLMMs) with negative binomial errors to all four acoustic indicators.','12, 20','Richness was underdispersed, not overdispersed; correct the distribution description')
append(44,'In models combining data from all five projects, project was included as a random intercept, with site and date nested within project.','10, 20')
change(46,'For the fish recruitment cuescape, an additional term was added to account for the variability in length of nighttime periods across locations between projects and dates by passing the log of the maximum possible score for each night','For the fish recruitment cuescape, an exposure term accounted for recorded nighttime effort by passing the log of recorded nighttime hours','7, 8')
change(46,'(v1.35.5)','(v2.0.6)','20','Original version string cannot be verified; exact current version is in R_session.txt')
append(46,'Habitat-type comparisons are reported using two-sided Wald tests and 95% confidence intervals. Sensitivity analyses included excluding Indonesia, replacing the project random intercept with fixed project effects, and examining available site-level covariates using matched subsets (Supplementary 1, Text S1). Age analyses were restricted to mid-stage restored sites and supplemented with a site-level permutation test using the six Indonesian sites.','9, 10, 11b, 13, 19, 20')
change(50,'(β = 0.144, p = 0.75), indicating similar overall values','(β = 0.144, p = 0.75), although a non-significant difference does not establish equivalence','9')
change(50,'16% (β = 0.15, p = 0.007)','24% (β = 0.22, p = 0.015)','12','Update phonic richness estimates and associated statistics')
change(50,'24% (β = 0.22, p = 0.009) and 21% (β = 0.19, p < 0.004)','35% (β = 0.30, p = 0.018) and 29% (β = 0.25, p = 0.019)','12','Update phonic richness estimates and associated statistics')
change(50,'74% decrease (β = - 1.29, p = 0.025)','73% decrease (β = -1.30, p = 0.025)','7, 8','Update raw-count effort-offset model')
change(50,'160% (β = 0.96, p = 0.024)','160% (β = 0.96, p = 0.025)','7, 8')
append(50,'After excluding Indonesia, the estimated directions of the principal habitat contrasts were retained, but none of the contrasts against degraded reefs had a 95% confidence interval excluding no difference. In both four-minute subsamples, phonic richness remained higher on healthy, early-stage and mid-stage restored reefs than on degraded reefs (all p < 0.03; Supplementary 1, Text S3).','10, 12, 19, 20')
change(54,'18% reduction compared to degraded reefs (β = - 0.20, p < 0.001)','21% reduction compared to degraded reefs (β = -0.24, p < 0.001)','12')
change(54,'(β = - 1.33, p < 0.001)','(β = -1.31, p < 0.001)','7, 8')
change(54,'(β = - 0.97, p = 0.045)','(β = -0.96, p = 0.045)','7, 8')
change(55,'18% (β = 0.16, p = 0.008) and 22% (β = 0.20, p = 0.001)','22% (β = 0.20, p = 0.008) and 27% (β = 0.24, p < 0.001)','12')
change(55,'496% increase (β = 1.79, p = 0.004)','495% increase (β = 1.78, p = 0.004)','7, 8')
change(55,'near significant 214% increase (β = 1.15, p = 0.063)','non-significant 214% increase (β = 1.14, p = 0.063)','7, 8, 9')
change(56,'72% higher phonic richness score (β = 0.54, p = 0.016)','106% higher phonic richness score (β = 0.72, p = 0.016)','12')
change(56,'snapping shrimp activity on the healthy reefs, which was 1669% higher (β = 2.87, p = 0.0022)','snapping shrimp activity on the mid-stage restored reef, which was 1716% higher (β = 2.90, p = 0.0017)','7, 8, 17','Correct habitat attribution: original coefficient was treatmentrestored, not treatmenthealthy')
change(57,'Phonic richness was 77% (β = 0.57, p < 0.001) and 131% (β = 0.84, p < 0.001) higher on healthy and early‐stage restoration reefs.','Phonic richness was 240% higher on the early-stage restoration reef (β = 1.23, p = 0.007), whereas the estimated 90% increase on healthy reefs was not significant (β = 0.64, p = 0.131).','12, 17')
append(62,'These are cross-sectional comparisons, and the contrasts across all projects were less precise when Indonesia was excluded. They therefore support context-dependent associations with restoration rather than uniform recovery across regions or direct causal effects.','1, 10, 19, 20')
append(63,'Repeated detections do not identify individual fish, and similarity to a healthy reference does not establish equivalence in fish diversity or ecosystem functioning.','7, 9')
change(64,'activity which contribute','activity which contributes','6','Grammar: singular subject agreement')
change(64,'This initial reduction may be explained by the disturbance to the substrate post-installation of reefs stars which stabilise and cover the benthic habitat inhabited by snapping shrimp.','This initial reduction may be explained by the disturbance to the substrate post-installation of reef stars which stabilise and cover the benthic habitat inhabited by snapping shrimp. The influence of restoration intensity, including the number of reef stars deployed, would be valuable to assess, but reef-star numbers varied mainly among projects rather than among sites within projects, preventing their effects from being separated reliably from broader project-level differences.','6, 13a','Correct noun modifier and explain why restoration intensity could not be tested independently')
change(64,'Parrotfish grazing was the only function which did not report a meaningful difference across any habitat type.','Parrotfish grazing was the only indicator without a significant habitat contrast in the analysis across all projects.','9','Avoid contradicting Australian project-specific results')
change(65,'The only acoustic indicator that differed between healthy and degraded reefs in the Mexican project was phonic richness, with no recovery for this observed on the early or mid-stage restored reefs.','In the Mexican project, no acoustic indicator differed significantly between healthy and degraded reefs; snapping shrimp activity was higher on the mid-stage restored reef than on degraded reefs.','10, 17','Reconcile Discussion with the stored coefficients and revised Results')
append(65,'Healthy sites were local reference conditions rather than undisturbed reefs. The causes of degradation reported in Table S1 provide context, but ongoing exposure at each reference site was not quantified. In the Maldives, fish biomass and richness informed site selection, so the acoustic contrasts are not an independent validation of differences in those same attributes.','14, 15')
append(67,'Direct field measurements would strengthen validation of the acoustic indicators against the processes they are intended to represent. In particular, neither grazing detections nor shrimp snaps provide a calibrated measure of carbonate removal or sediment turnover. Restoration outcomes in Kenya and the Maldives were each represented by one early-stage site, and in Australia by one mid-stage site. These findings are preliminary site-specific comparisons: repeated recording days do not provide independent spatial replication.','16, 17')
change(67,'Long-term temporal sampling on the scale of years could be used to better understand the timeframe over which recovery occurs and how this differs across inferred functions through early to late-stage recovery.','Long-term temporal sampling on the scale of years could be used to better understand the time frame over which recovery occurs and how this differs across inferred functions through early to late-stage recovery. Across the eight mid-stage sites, no clear age pattern was observed (Supplementary 1, Table S9), although limited spatial replication means we cannot determine whether age effects are truly absent.','11b')
change(59,'Local impacts of restoration on ecosystem process indicators','Local impacts of restoration on ecosystem process indicators.','6','Grammar: add missing caption punctuation')
(ROOT/'outputs/planned_edits.json').write_text(json.dumps(EDITS,indent=2,ensure_ascii=False))
print(f'{len(EDITS)} checked edits')
