"""Propagate the final date-within-project model through resubmission files."""

from __future__ import annotations

from copy import deepcopy
from datetime import datetime, timezone
from pathlib import Path
from tempfile import NamedTemporaryFile
from zipfile import ZIP_DEFLATED, ZipFile

import pandas as pd
from lxml import etree


ROOT = Path(__file__).resolve().parents[1]
RESUBMISSION = ROOT / "resubmission"
W = "http://schemas.openxmlformats.org/wordprocessingml/2006/main"
M = "http://schemas.openxmlformats.org/officeDocument/2006/math"
R = "http://schemas.openxmlformats.org/officeDocument/2006/relationships"
XML = "http://www.w3.org/XML/1998/namespace"
NS = {"w": W, "m": M, "r": R}


def wtag(name: str) -> str:
    return f"{{{W}}}{name}"


def mtag(name: str) -> str:
    return f"{{{M}}}{name}"


def accepted_text(element: etree._Element) -> str:
    """Return visible text, including insertions and excluding deletions."""
    return "".join(
        element.xpath(
            ".//w:t[not(ancestor::w:del)]/text()",
            namespaces=NS,
        )
    )


def load_package(path: Path) -> tuple[dict[str, bytes], etree._Element]:
    with ZipFile(path) as archive:
        parts = {name: archive.read(name) for name in archive.namelist()}
    return parts, etree.fromstring(parts["word/document.xml"])


def write_package(path: Path, parts: dict[str, bytes], root: etree._Element) -> None:
    parts["word/document.xml"] = etree.tostring(
        root,
        xml_declaration=True,
        encoding="UTF-8",
        standalone=True,
    )
    with NamedTemporaryFile(dir=path.parent, suffix=".docx", delete=False) as handle:
        temporary = Path(handle.name)
    try:
        with ZipFile(temporary, "w", ZIP_DEFLATED) as archive:
            for name, value in parts.items():
                archive.writestr(name, value)
        temporary.replace(path)
    finally:
        temporary.unlink(missing_ok=True)


def make_run(text: str, deleted: bool = False) -> etree._Element:
    run = etree.Element(wtag("r"))
    node = etree.SubElement(run, wtag("delText" if deleted else "t"))
    node.text = text
    node.set(f"{{{XML}}}space", "preserve")
    return run


def replace_clean_paragraph(paragraph: etree._Element, text: str) -> None:
    properties = paragraph.find(wtag("pPr"))
    for child in list(paragraph):
        if child is not properties:
            paragraph.remove(child)
    paragraph.append(make_run(text))


def next_change_id(root: etree._Element) -> int:
    values = []
    for value in root.xpath("//@w:id", namespaces=NS):
        try:
            values.append(int(value))
        except ValueError:
            continue
    return max(values, default=0) + 1


def revision_wrapper(kind: str, change_id: int, child: etree._Element) -> etree._Element:
    wrapper = etree.Element(wtag(kind))
    wrapper.set(wtag("id"), str(change_id))
    wrapper.set(wtag("author"), "Revision")
    wrapper.set(wtag("date"), datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ"))
    wrapper.append(child)
    return wrapper


def replace_tracked_paragraph(
    paragraph: etree._Element,
    text: str,
    change_id: int,
) -> int:
    old = accepted_text(paragraph)
    properties = paragraph.find(wtag("pPr"))
    for child in list(paragraph):
        if child is not properties:
            paragraph.remove(child)
    paragraph.append(revision_wrapper("del", change_id, make_run(old, deleted=True)))
    paragraph.append(revision_wrapper("ins", change_id + 1, make_run(text)))
    return change_id + 2


def append_paragraph_after(
    paragraph: etree._Element,
    text: str,
    tracked: bool,
    change_id: int,
) -> int:
    new = etree.Element(wtag("p"), nsmap=paragraph.nsmap)
    properties = paragraph.find(wtag("pPr"))
    if properties is not None:
        new.append(deepcopy(properties))
    run = make_run(text)
    if tracked:
        new.append(revision_wrapper("ins", change_id, run))
        change_id += 1
    else:
        new.append(run)
    paragraph.addnext(new)
    return change_id


def math_run(text: str) -> etree._Element:
    run = etree.Element(mtag("r"))
    node = etree.SubElement(run, mtag("t"))
    node.text = text
    node.set(f"{{{XML}}}space", "preserve")
    return run


def math_subscript(base: str, subscript: str) -> etree._Element:
    element = etree.Element(mtag("sSub"))
    base_element = etree.SubElement(element, mtag("e"))
    base_element.append(math_run(base))
    sub_element = etree.SubElement(element, mtag("sub"))
    sub_element.append(math_run(subscript))
    return element


def math_parenthesised_subscript(base: str, subscript: str) -> etree._Element:
    delimiter = etree.Element(mtag("d"))
    properties = etree.SubElement(delimiter, mtag("dPr"))
    etree.SubElement(properties, mtag("begChr")).set(mtag("val"), "(")
    etree.SubElement(properties, mtag("endChr")).set(mtag("val"), ")")
    expression = etree.SubElement(delimiter, mtag("e"))
    expression.append(math_subscript(base, subscript))
    return delimiter


def replace_equation(paragraph: etree._Element) -> None:
    properties = paragraph.find(wtag("pPr"))
    for child in list(paragraph):
        if child is not properties:
            paragraph.remove(child)
    outer = etree.SubElement(paragraph, mtag("oMathPara"))
    outer_properties = etree.SubElement(outer, mtag("oMathParaPr"))
    etree.SubElement(outer_properties, mtag("jc")).set(mtag("val"), "center")
    equation = etree.SubElement(outer, mtag("oMath"))
    equation.append(math_run("log"))
    equation.append(math_parenthesised_subscript("μ", "jtk"))
    equation.append(math_run(" = "))
    equation.append(math_subscript("β", "0"))
    equation.append(math_run(" + "))
    equation.append(math_subscript("β", "m"))
    equation.append(math_run(" + "))
    equation.append(math_subscript("u", "k"))
    equation.append(math_run(" + "))
    equation.append(math_subscript("v", "jk"))
    equation.append(math_run(" + "))
    equation.append(math_subscript("w", "tk"))
    equation.append(math_run(" + log"))
    equation.append(math_parenthesised_subscript("E", "jtk"))


def replace_in_visible_nodes(element: etree._Element, old: str, new: str) -> None:
    matches = [
        node
        for node in element.xpath(".//w:t[not(ancestor::w:del)]", namespaces=NS)
        if old in (node.text or "")
    ]
    if len(matches) != 1:
        raise ValueError(f"Expected one visible occurrence of {old!r}, found {len(matches)}")
    matches[0].text = matches[0].text.replace(old, new)


def find_paragraph(root: etree._Element, prefix: str) -> etree._Element:
    paragraphs = root.xpath("//w:body/w:p", namespaces=NS)
    matches = [paragraph for paragraph in paragraphs if accepted_text(paragraph).startswith(prefix)]
    if len(matches) != 1:
        raise ValueError(f"Expected one paragraph beginning {prefix!r}, found {len(matches)}")
    return matches[0]


METHOD_1 = (
    "To reflect the hierarchical structure of the data, we fitted generalised linear mixed "
    "models (GLMMs) with negative binomial errors to each of the four acoustic indicators. "
    "Habitat type was included as a fixed effect. In models combining data from all five "
    "projects, project was included as a random intercept, with site nested within project to "
    "account for repeated observations from the same site. Date was also nested within project "
    "to account for conditions shared among sites recorded on the same day, such as weather or "
    "lunar phase. Each project was located in a different country, meaning project and country "
    "were equivalent in these models."
)

METHOD_2 = (
    "Here, μⱼₜₖ is the expected count at site j, on date t, within project k. β₀ is the "
    "model intercept and βₘ represents the effect of habitat type m relative to degraded reefs. "
    "The random effects uₖ, vⱼₖ and wₜₖ account for variation among projects, among sites within "
    "projects and among dates within projects, respectively. Eⱼₜₖ represents recording effort. "
    "For grazing and shrimp activity, this was the number of hours recorded, while for the "
    "recruitment cuescape it was the number of nighttime hours recorded. For phonic richness, "
    "Eⱼₜₖ was set to one, which is equivalent to fitting no exposure offset. For models fitted "
    "separately to each project, the project effect was removed and site and date were included "
    "directly as random effects."
)

METHOD_3 = (
    "All models were implemented in R using the lme4 package (v2.0.6). We compared each habitat "
    "type with degraded reefs using two-sided Wald tests and 95% confidence intervals. Differences "
    "were considered statistically significant at p < 0.05, corresponding to a 95% confidence "
    "interval that excluded zero. To test whether the conclusions depended on uneven sampling "
    "among projects or on the chosen model structure, we repeated the analyses after excluding "
    "Indonesia and after treating project as a fixed rather than random effect (Supplementary 1, "
    "Text S1; Tables S3 and S6). We also tested whether recording effort affected phonic richness "
    "(Table S5) and how the habitat comparisons changed when coral cover was included for sites "
    "with available measurements (Table S7). To explore possible relationships with restoration "
    "age, we compared the eight mid-stage sites and used an exact permutation test for the six "
    "Indonesian sites, the only project with sufficient mid-stage sites for a within-project "
    "comparison (Tables S8 and S9)."
)


def update_manuscript(path: Path, tracked: bool) -> None:
    parts, root = load_package(path)
    paragraphs = root.xpath("//w:body/w:p", namespaces=NS)
    change_id = next_change_id(root)

    replace_in_visible_nodes(
        paragraphs[43],
        "refitted the pooled model",
        "refitted the model combining all projects",
    )
    replace_in_visible_nodes(
        find_paragraph(root, "Our results indicate"),
        "the pooled contrasts",
        "the contrasts across all projects",
    )
    replace_in_visible_nodes(
        find_paragraph(root, "In addition, we find evidence"),
        "in the pooled analysis",
        "in the analysis across all projects",
    )
    if tracked:
        change_id = replace_tracked_paragraph(paragraphs[48], METHOD_1, change_id)
        change_id = replace_tracked_paragraph(paragraphs[50], METHOD_2, change_id)
    else:
        replace_clean_paragraph(paragraphs[48], METHOD_1)
        replace_clean_paragraph(paragraphs[50], METHOD_2)
    replace_equation(paragraphs[49])
    append_paragraph_after(paragraphs[50], METHOD_3, tracked, change_id)

    result_replacements = {
        "400% increase (β = 1.61": "361% increase (β = 1.53",
        "(β = 0.144, p = 0.75)": "(β = 0.22, p = 0.96)",
    }
    if tracked:
        result_replacements.update(
            {
                "333%": "271%",
                "(β = 1.47": "(β = 1.31",
            }
        )
    else:
        result_replacements["333% higher value (β = 1.47"] = (
            "271% higher value (β = 1.31"
        )
    for old, new in result_replacements.items():
        replace_in_visible_nodes(paragraphs[54], old, new)

    figure = ROOT / "outputs" / "figures" / "figure4_revised.png"
    if tracked:
        image_part = "word/media/revision_figure_51.png"
    else:
        image_part = "word/media/image5.png"
    if image_part not in parts:
        raise ValueError(f"Expected Figure 4 part {image_part} in {path.name}")
    parts[image_part] = figure.read_bytes()
    write_package(path, parts, root)


def set_paragraph_text(paragraph: etree._Element, text: str, tracked: bool) -> None:
    existing_insertion = None
    if tracked:
        insertions = paragraph.xpath(".//w:ins", namespaces=NS)
        if insertions:
            existing_insertion = insertions[0]
    properties = paragraph.find(wtag("pPr"))
    for child in list(paragraph):
        if child is not properties:
            paragraph.remove(child)
    run = make_run(text)
    if tracked:
        if existing_insertion is not None:
            wrapper = etree.Element(wtag("ins"), attrib=dict(existing_insertion.attrib))
            wrapper.append(run)
        else:
            wrapper = revision_wrapper("ins", 900000 + len(text), run)
        paragraph.append(wrapper)
    else:
        paragraph.append(run)


def find_table(root: etree._Element, first_header: str) -> etree._Element:
    tables = root.xpath("//w:body/w:tbl", namespaces=NS)
    matches = []
    for table in tables:
        rows = table.findall(wtag("tr"))
        if not rows:
            continue
        cells = rows[0].findall(wtag("tc"))
        if cells and accepted_text(cells[0]).strip() == first_header:
            matches.append(table)
    if len(matches) != 1:
        raise ValueError(f"Expected one table headed {first_header!r}, found {len(matches)}")
    return matches[0]


def set_cell_text(cell: etree._Element, text: str, tracked: bool) -> None:
    paragraphs = cell.findall(wtag("p"))
    if not paragraphs:
        paragraph = etree.SubElement(cell, wtag("p"))
    else:
        paragraph = paragraphs[0]
        for extra in paragraphs[1:]:
            cell.remove(extra)
    set_paragraph_text(paragraph, text, tracked)


def format_result(metric: str, scenario: str, term: str) -> str:
    path = ROOT / "outputs" / "analysis" / f"{metric}__{scenario}_coefficients.csv"
    data = pd.read_csv(path)
    row = data[data.term == term].iloc[0]
    return f"{row.estimate:.2f} [{row.lower95:.2f}, {row.upper95:.2f}]; p={row.p:.3g}"


def update_supplement(path: Path, tracked: bool) -> None:
    parts, root = load_package(path)
    paragraph_updates = {
        "Finalised models:": (
            "Finalised models: The following checks assess the finalised models (Table S3, “All "
            "projects”; sample sizes in Table S4). All four models included habitat type as a fixed "
            "effect and random intercepts for project, site within project and date within project. "
            "To account for unequal recording time, grazing and shrimp counts were modelled as "
            "negative binomial counts with log(recorded hours) as an offset, estimating detections "
            "per hour of recording time available. Recruitment cuescape models used log(recorded "
            "nighttime hours) as the offset. These measure detection rates, not animal abundance. "
            "Grazing models excluded Kenya, where the grazing sonotype was not detected."
        ),
        "Recording effort and richness:": (
            "Recording effort and richness: To test whether longer recording time increased observed "
            "richness, we reduced Indonesia’s schedule from one minute every two minutes to one every "
            "four, matching other projects. We tested both alternate-recording subsets separately, "
            "retaining whole one-minute recordings. Days required at least 324 of 360 scheduled "
            "recordings; both comparisons retained the same 1,060 site-days. After recalculating "
            "richness and refitting the model combining all projects, all three habitat comparisons "
            "against degraded reefs kept the same positive direction and remained statistically "
            "significant in both subsets. Thus, downsampling did not change either the direction or "
            "significance of these comparisons (all p < 0.03; Table S5)."
        ),
        "Indonesia’s influence:": (
            "Indonesia’s influence: To test whether Indonesia’s larger sample (21 of 45 sites; Table "
            "S4) strongly influenced findings across projects, we refitted all four indicators without "
            "it. Every habitat comparison against degraded reefs kept the same direction, but "
            "previously significant differences became non-significant; the other comparisons "
            "remained non-significant. All 95% confidence intervals included no difference (Table S3). "
            "Including Indonesia adds site replication, which can improve precision and increase the "
            "chance of detecting real habitat differences as statistically significant. This check "
            "cannot separate that contribution from project-specific differences."
        ),
        "Model assumptions:": (
            "Model assumptions: Richness varied less than the negative binomial model assumes. The "
            "date-within-project variance was effectively zero in the finalised phonic-richness model "
            "(2.62 × 10⁻¹³) and zero in the richness model with fixed project effects. In the finalised "
            "shrimp model, date-within-project variance was small (0.0031), but did not collapse to "
            "zero. We retained date effects in all four finalised models because our model structure "
            "was ecologically motivated; recordings from the same project and date may share "
            "environmental conditions. Date effects accounted for variation in several models, so "
            "retaining them provided a consistent approach rather than selecting terms separately for "
            "each indicator. To test an alternative way of accounting for differences among projects, "
            "we fitted project as a fixed rather than random effect. Every habitat comparison against "
            "degraded reefs kept the same direction, with significant differences remaining "
            "significant and non-significant differences remaining non-significant at p < 0.05 "
            "(Table S6)."
        ),
        "Table S3. Pooled": "Table S3. Across-project log response ratios [95% CI] against degraded reefs",
        "Each alternative changes": (
            "All indicators use negative binomial models. The alternative models treat project as a "
            "fixed rather than random effect."
        ),
    }
    for prefix, text in paragraph_updates.items():
        set_paragraph_text(find_paragraph(root, prefix), text, tracked)

    terms = ["treatmenthealthy", "treatmentnewly_restored", "treatmentrestored"]
    main_tables = [
        table
        for table in root.xpath("//w:body/w:tbl", namespaces=NS)
        if "All projects" in accepted_text(table.find(wtag("tr")))
    ]
    if len(main_tables) != 1:
        raise ValueError(f"Expected one across-project table, found {len(main_tables)}")
    main_table = main_tables[0]
    rows = main_table.findall(wtag("tr"))
    for offset, term in enumerate(terms, start=1):
        cells = rows[offset].findall(wtag("tc"))
        set_cell_text(cells[2], format_result("settlement_cuescape", "baseline", term), tracked)
        set_cell_text(
            cells[3],
            format_result("settlement_cuescape", "exclude_indonesia", term),
            tracked,
        )

    assumptions_table = None
    for table in root.xpath("//w:body/w:tbl", namespaces=NS):
        first_row = table.find(wtag("tr"))
        if first_row is not None and "Finalised model" in accepted_text(first_row):
            assumptions_table = table
            break
    if assumptions_table is None:
        raise ValueError("Could not find the model-assumptions table")
    rows = assumptions_table.findall(wtag("tr"))
    for offset, term in enumerate(terms, start=1):
        cells = rows[offset].findall(wtag("tc"))
        set_cell_text(cells[2], format_result("settlement_cuescape", "baseline", term), tracked)
        set_cell_text(cells[3], format_result("settlement_cuescape", "country_fixed", term), tracked)
    for row in rows:
        cells = row.findall(wtag("tc"))
        if len(cells) == 5:
            row.remove(cells[4])
    grid = assumptions_table.find(wtag("tblGrid"))
    if grid is not None:
        columns = grid.findall(wtag("gridCol"))
        if len(columns) == 5:
            grid.remove(columns[4])

    cover_table = None
    for table in root.xpath("//w:body/w:tbl", namespaces=NS):
        first_row = table.find(wtag("tr"))
        if first_row is not None and "Without cover" in accepted_text(first_row):
            cover_table = table
            break
    if cover_table is None:
        raise ValueError("Could not find the coral-cover table")
    rows = cover_table.findall(wtag("tr"))
    scenarios = [
        "cover_complete_baseline",
        "cover_bound_0",
        "cover_bound_2.5",
        "cover_bound_5",
    ]
    for offset, term in enumerate(terms, start=1):
        cells = rows[offset].findall(wtag("tc"))
        for column, scenario in enumerate(scenarios, start=2):
            set_cell_text(
                cells[column],
                format_result("settlement_cuescape", scenario, term),
                tracked,
            )
    write_package(path, parts, root)


def update_response(path: Path) -> None:
    parts, root = load_package(path)
    replacements = {
        "Response: This is addressed in response to comment 7.": (
            "Response: This is addressed in response to comment 7. Shrimp counts are now modelled "
            "with log(recorded hours) as an exposure offset. The across-project habitat comparisons "
            "against degraded reefs retained the same directions and significance decisions: "
            "significant differences remained significant and non-significant differences remained "
            "non-significant. See Supp. Text S1, “Finalised models”, with the finalised estimates in "
            "Table S3."
        ),
        "Response: We agree that project random effects": (
            "Response: We agree that project random effects do not rule out disproportionate "
            "influence. Indonesia contributed 21 of the 45 sites. We refitted all four indicators "
            "without Indonesia and describe this check in our new sensitivity analysis section: "
            "Supp. Text S1, “Indonesia’s influence”, with estimates, 95% intervals and p-values in "
            "Table S3. Every habitat comparison against degraded reefs kept the same direction, but "
            "previously significant differences became non-significant; the other comparisons "
            "remained non-significant. All 95% confidence intervals included zero. For example, the "
            "mid-stage cuescape coefficient changed from 1.53 (p < 0.001) to 0.99 (p = 0.251). "
            "Including Indonesia adds site replication, which can improve precision and increase the "
            "chance of detecting real habitat differences as statistically significant. This check "
            "cannot separate that contribution from project-specific differences. We therefore retain "
            "the qualification on generalisation across regions."
        ),
        "Response: Although phonic richness": (
            "Response: Although phonic richness was not multiplied by duty cycle, we agree its a very "
            "valid point that additional recording time may in fact increase observed richness. To "
            "test this, we reduced the Indonesian recordings from one minute every two minutes to one "
            "every four, matching other projects. We tested both alternate-recording subsets "
            "separately, recalculated daily richness and refitted the model combining all projects "
            "using the same site-days. All three across-project habitat comparisons against degraded "
            "reefs kept the same positive direction and remained statistically significant in both "
            "subsamples. Downsampling therefore changed neither the direction nor the significance of "
            "these comparisons (all p < 0.03; Table S5). See our new sensitivity analysis section in "
            "Supp. Text S1 where this is included under “Recording effort and richness”, and Table S5. "
            "The finalised models retain full recording effort, we treat the reduced-effort datasets "
            "as sensitivity checks."
        ),
        "Response to 13c:": (
            "Response to 13c: As detailed in our responses to comments 7–8, 10, 11b, 12, 13b, "
            "19b–c and 20, we refitted the detection-count models using recording-effort offsets and "
            "conducted additional analyses examining Indonesia’s influence, recording effort for "
            "phonic richness, restoration age, project effects and coral cover. These analyses are "
            "briefly described in the main manuscript Methods (clean manuscript lines 395–399 and "
            "413–417), with the principal sensitivity results reported at lines 446–450. Their "
            "rationale, methods and full results are presented in Supplementary Text S1 and Tables "
            "S3–S9."
        ),
    }
    for prefix, text in replacements.items():
        set_paragraph_text(find_paragraph(root, prefix), text, tracked=False)
    write_package(path, parts, root)


def main() -> None:
    update_manuscript(RESUBMISSION / "Manuscript_clean.docx", tracked=False)
    update_manuscript(RESUBMISSION / "Manuscript_tracked.docx", tracked=True)
    update_supplement(RESUBMISSION / "Supplementary_1_clean.docx", tracked=False)
    update_supplement(RESUBMISSION / "Supplementary_1_tracked.docx", tracked=True)
    update_response(RESUBMISSION / "Response_to_reviewers_and_editors.docx")


if __name__ == "__main__":
    main()
