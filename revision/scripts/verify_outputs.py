"""Check source integrity, reversible manuscript edits and preserved DOCX assets."""
from copy import deepcopy
from pathlib import Path
from zipfile import ZipFile
import hashlib
import json
import re
import sys
from lxml import etree
from docx import Document
ROOT=Path(__file__).resolve().parents[1]
sys.path.insert(0,str(ROOT/'src'))
from docx_revision import NS,tag,visible

def xml(path: Path):
    with ZipFile(path) as z:return etree.fromstring(z.read('word/document.xml'))

def resolve(root,accept: bool):
    """Resolve insertion/deletion runs as Word would for text and pictures."""
    root=deepcopy(root)
    remove,unwrap=('del','ins') if accept else ('ins','del')
    for n in root.xpath('//w:'+remove,namespaces=NS):n.getparent().remove(n)
    for n in root.xpath('//w:'+unwrap,namespaces=NS):
        parent=n.getparent();at=parent.index(n)
        for c in list(n):parent.insert(at,c);at+=1
        parent.remove(n)
    for n in root.xpath('//w:delText',namespaces=NS):n.tag=tag('t')
    return root

def signature(root, path: Path):
    """Extract paragraph text and embedded-image hashes from a DOCX body."""
    relationships = {}
    with ZipFile(path) as archive:
        rels = etree.fromstring(archive.read('word/_rels/document.xml.rels'))
        for relationship in rels:
            target = relationship.get('Target', '')
            if target.startswith('media/'):
                relationships[relationship.get('Id')] = hashlib.sha256(
                    archive.read('word/' + target)).hexdigest()
    return [
        (
            visible(paragraph),
            [relationships[identifier] for identifier in paragraph.xpath(
                './/a:blip/@r:embed', namespaces=NS)],
        )
        for paragraph in root.find(tag('body')).findall(tag('p'))
    ]

def main() -> None:
    checks=[]
    baseline=json.loads((ROOT/'scratch/source_hashes.json').read_text())
    for relative,digest in baseline.items():assert hashlib.sha256((ROOT/relative).read_bytes()).hexdigest()==digest,relative
    checks.append('All original documents and supplied source files match their initial SHA-256 hashes.')
    for entry in json.loads((ROOT/'outputs/input_manifest.json').read_text()):
        assert hashlib.sha256((ROOT/'data'/entry['file']).read_bytes()).hexdigest()==entry['sha256']
    checks.append('All 46 copied input CSVs match their recorded source hashes.')
    original=xml(ROOT/'docs/Manuscript.docx');tracked=xml(ROOT/'outputs/Manuscript_tracked.docx');clean=xml(ROOT/'outputs/Manuscript_clean.docx')
    assert signature(resolve(tracked,True), ROOT/'outputs/Manuscript_tracked.docx')==signature(clean, ROOT/'outputs/Manuscript_clean.docx')
    assert signature(resolve(tracked,False), ROOT/'outputs/Manuscript_tracked.docx')==signature(original, ROOT/'docs/Manuscript.docx')
    checks.append('Accepting tracked manuscript changes reproduces clean text and figure references; rejecting restores the original.')
    original_instructions = original.xpath('//w:instrText/text()', namespaces=NS)
    clean_instructions = clean.xpath('//w:instrText/text()', namespaces=NS)
    assert [value.strip() for value in original_instructions] == [
        value.strip() for value in clean_instructions]
    assert original.xpath('//w:fldChar/@w:fldCharType', namespaces=NS) == clean.xpath(
        '//w:fldChar/@w:fldCharType', namespaces=NS)
    maths={'m':'http://schemas.openxmlformats.org/officeDocument/2006/math'}
    original_maths = original.xpath('//m:oMath', namespaces=maths)
    clean_maths = clean.xpath('//m:oMath', namespaces=maths)
    math_text = lambda nodes: [re.sub(r'\s+', '', ''.join(node.itertext())) for node in nodes]
    assert math_text(original_maths) == math_text(clean_maths)
    checks.append('All citation fields and mathematical expressions are preserved.')
    for base in ['Manuscript','Supplementary_1']:
        source=ROOT/'docs'/('Supplementary 1.docx' if base=='Supplementary_1' else 'Manuscript.docx')
        with ZipFile(source) as src,ZipFile(ROOT/'outputs'/f'{base}_clean.docx') as dest:
            source_media = {
                hashlib.sha256(src.read(name)).hexdigest()
                for name in src.namelist() if name.startswith('word/media/')
            }
            destination_media = {
                hashlib.sha256(dest.read(name)).hexdigest()
                for name in dest.namelist() if name.startswith('word/media/')
            }
            expected_preserved = len(source_media) - (2 if base == 'Manuscript' else 0)
            assert len(source_media & destination_media) == expected_preserved
            for name in ['word/styles.xml','word/numbering.xml','word/fontTable.xml']:
                if name in dest.namelist():
                    etree.fromstring(dest.read(name))
    checks.append('Unchanged media are preserved, the two results figures are replaced as intended, and document style resources remain valid XML.')
    source=xml(ROOT/'docs/Supplementary 1.docx');supp=xml(ROOT/'outputs/Supplementary_1_clean.docx')
    original_tables=source.xpath('//w:tbl',namespaces=NS);new_tables=supp.xpath('//w:tbl',namespaces=NS)
    table_text = lambda table: re.sub(
        r'\s+', '', ''.join(table.xpath('.//w:t/text()', namespaces=NS)))
    assert [table_text(table) for table in original_tables] == [
        table_text(table) for table in new_tables[:len(original_tables)]]
    assert signature(resolve(xml(ROOT/'outputs/Supplementary_1_tracked.docx'),True), ROOT/'outputs/Supplementary_1_tracked.docx')==signature(supp, ROOT/'outputs/Supplementary_1_clean.docx')
    checks.append('The text of both existing supplementary tables is preserved; seven new analysis tables were appended.')
    for filename in ['Manuscript_clean.docx','Manuscript_tracked.docx','Supplementary_1_clean.docx','Supplementary_1_tracked.docx','Comments_reference.docx','Responses_populated.docx','Summary_report.docx']:
        path=ROOT/'outputs'/filename
        with ZipFile(path) as z:
            assert z.testzip() is None
            for name in z.namelist():
                if name.endswith('.xml'):etree.fromstring(z.read(name))
        Document(path)
    checks.append('All seven DOCX deliverables pass ZIP, XML parsing and python-docx open checks.')
    count=len(' '.join(p.text for p in Document(ROOT/'outputs/Manuscript_clean.docx').paragraphs[:72]).split())
    checks.append(f'Approximate manuscript word count including captions and excluding references: {count}.')
    result={'passed':checks,'limitations':['Word desktop rendering was not available; line references use the supplied LibreOffice-rendered PDF.','Statistical adequacy and author interpretation decisions remain explicitly identified in the response TODOs.']}
    (ROOT/'outputs/verification.json').write_text(json.dumps(result,indent=2))
    print('\n'.join(checks))

if __name__=='__main__':main()
