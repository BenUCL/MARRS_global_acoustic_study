"""Apply reversible WordprocessingML edits without round-tripping document assets."""
from copy import deepcopy
from datetime import datetime, timezone
from pathlib import Path
from zipfile import ZipFile, ZIP_DEFLATED
from lxml import etree

W='http://schemas.openxmlformats.org/wordprocessingml/2006/main'
R='http://schemas.openxmlformats.org/officeDocument/2006/relationships'
NS={'w':W,'r':R,'a':'http://schemas.openxmlformats.org/drawingml/2006/main'}
XML='http://www.w3.org/XML/1998/namespace'

def tag(name: str) -> str:
    return '{'+W+'}'+name

def visible(p) -> str:
    """Read accepted text, including inserted and excluding deleted characters."""
    return ''.join(p.xpath('.//w:t[not(ancestor::w:del)]/text()',namespaces=NS))

class RevisionDocument:
    """Keep original ZIP parts verbatim unless an edit explicitly changes them."""
    def __init__(self, source: Path):
        with ZipFile(source) as z:
            self.parts={n:z.read(n) for n in z.namelist()}
        self.root=etree.fromstring(self.parts['word/document.xml'])
        self.paragraphs=self.root.find(tag('body')).findall(tag('p'))
        self.change_id=1
        self.date=datetime.now(timezone.utc).strftime('%Y-%m-%dT%H:%M:%SZ')
        self.comments=etree.Element(tag('comments'),nsmap={'w':W})
        self.log=[]

    def wrapper(self, kind: str, child):
        """Create an anonymous revision wrapper with a unique identifier."""
        node=etree.Element(tag(kind))
        for key,value in {'id':str(self.change_id),'author':'Revision','date':self.date}.items():node.set(tag(key),value)
        self.change_id+=1;node.append(child);return node

    def replace(self, index: int, old: str, new: str, comments: str, reason: str):
        """Replace a unique text span while retaining run formatting and fields."""
        p=self.paragraphs[index];text=visible(p)
        if old=='':
            run=etree.Element(tag('r'))
            previous=p.xpath('.//w:r[w:t]',namespaces=NS)
            if previous and previous[-1].find(tag('rPr')) is not None:
                run.append(deepcopy(previous[-1].find(tag('rPr'))))
            t=etree.SubElement(run,tag('t'));t.text=new;t.set('{'+XML+'}space','preserve')
            p.append(self.wrapper('ins',run))
            self.log.append(dict(paragraph=index,old=old,new=new,comments=comments,reason=reason))
            return
        if text.count(old)!=1:raise ValueError(f'Anchor missing or ambiguous at paragraph {index}: {old[:70]}')
        start=text.index(old);end=start+len(old);position=0;first=True
        nodes=p.xpath('.//w:t[not(ancestor::w:del)]',namespaces=NS)
        for node in nodes:
            value=node.text or '';a=position;b=a+len(value);position=b
            if b<=start or a>=end:continue
            run=node.getparent()
            if run.tag!=tag('r') or len(run.findall(tag('t')))!=1:
                raise ValueError('Complex run requires an explicit edit')
            if any(c.tag not in {tag('rPr'),tag('t'),tag('lastRenderedPageBreak')} for c in run):
                raise ValueError('Refusing to overwrite a non-text field or drawing')
            parent=run.getparent()
            if parent.tag in {tag('ins'),tag('del')}:
                raise ValueError('Overlapping planned edits require consolidation')
            left=max(0,start-a);right=min(len(value),end-a)
            def piece(content: str, deleted: bool=False):
                result=deepcopy(run)
                for marker in result.findall(tag('lastRenderedPageBreak')): result.remove(marker)
                t=result.find(tag('t'));t.text=content
                t.set('{'+XML+'}space','preserve')
                if deleted:t.tag=tag('delText')
                return result
            pieces=[]
            if value[:left]:pieces.append(piece(value[:left]))
            pieces.append(self.wrapper('del',piece(value[left:right],True)))
            if first and new:pieces.append(self.wrapper('ins',piece(new)))
            if value[right:]:pieces.append(piece(value[right:]))
            where=parent.index(run);parent.remove(run)
            for replacement in pieces:
                parent.insert(where,replacement);where+=1
            first=False
        self.log.append(dict(paragraph=index,old=old,new=new,comments=comments,reason=reason))

    def add_comment(self, index: int, text: str):
        """Attach a Word comment to a revised paragraph."""
        p=self.paragraphs[index];cid=str(len(self.comments))
        comment=etree.SubElement(self.comments,tag('comment'),{tag('id'):cid,tag('author'):'Revision',tag('date'):self.date})
        cp=etree.SubElement(comment,tag('p'));r=etree.SubElement(cp,tag('r'));etree.SubElement(r,tag('t')).text=text
        start=etree.Element(tag('commentRangeStart'),{tag('id'):cid})
        end=etree.Element(tag('commentRangeEnd'),{tag('id'):cid})
        p.insert(1 if p.find(tag('pPr')) is not None else 0,start);p.append(end)
        run=etree.SubElement(p,tag('r'));etree.SubElement(run,tag('commentReference'),{tag('id'):cid})

    def replace_image(self,index: int,path: Path):
        """Track replacement of an embedded image while keeping its display size."""
        p=self.paragraphs[index];blip=p.xpath('.//a:blip',namespaces=NS)[0]
        run=blip
        while run.tag!=tag('r'):run=run.getparent()
        new=deepcopy(run);rels=etree.fromstring(self.parts['word/_rels/document.xml.rels'])
        rid='rIdRevisionFigure'+str(index)
        relns='http://schemas.openxmlformats.org/package/2006/relationships'
        etree.SubElement(rels,'{'+relns+'}Relationship',Id=rid,Type=R+'/image',Target=f'media/revision_figure_{index}.png')
        new.xpath('.//a:blip',namespaces=NS)[0].set('{'+R+'}embed',rid)
        from PIL import Image
        with Image.open(path) as picture: width,height=picture.size
        for crop in new.xpath('.//a:srcRect',namespaces=NS): crop.getparent().remove(crop)
        extent=new.xpath('.//*[local-name()="extent"]')[0]
        cx=int(extent.get('cx'));cy=int(extent.get('cy'))
        if abs((width/height)/(cx/cy)-1) > .01:
            raise ValueError('Replacement figure must match the original display aspect ratio')
        for extent in new.xpath('.//*[local-name()="extent"]|.//a:xfrm/a:ext',namespaces=NS):
            extent.set('cx',str(cx));extent.set('cy',str(cy))
        # Both old and new drawings coexist in the tracked document.
        # Give replacements unique non-visual IDs to avoid Word repair prompts.
        for properties in new.xpath('.//*[local-name()="docPr" or local-name()="cNvPr"]'):
            properties.set('id',str(2000000000+index))
        for description in new.xpath('.//*[@descr]'):
            description.set('descr','Habitat contrasts as percentage change over degraded reefs, with 75% and 95% confidence intervals; original figure style with updated model values.')

        self.parts[f'word/media/revision_figure_{index}.png']=path.read_bytes()
        self.parts['word/_rels/document.xml.rels']=etree.tostring(rels,xml_declaration=True,encoding='UTF-8',standalone=True)
        parent=run.getparent();where=parent.index(run);parent.remove(run)
        parent.insert(where,self.wrapper('del',run));parent.insert(where+1,self.wrapper('ins',new))

    def write(self,path: Path,clean: bool=False):
        """Write a clean accepted copy or a genuine tracked revision copy."""
        root=deepcopy(self.root);parts=dict(self.parts)
        if clean:
            for node in root.xpath('//w:del',namespaces=NS):node.getparent().remove(node)
            for node in root.xpath('//w:ins',namespaces=NS):
                parent=node.getparent();where=parent.index(node)
                for c in list(node):parent.insert(where,c);where+=1
                parent.remove(node)
            for node in root.xpath('//w:commentRangeStart|//w:commentRangeEnd|//w:commentReference',namespaces=NS):node.getparent().remove(node)
        else:
            settings=etree.fromstring(parts['word/settings.xml'])
            if settings.find(tag('trackRevisions')) is None:
                etree.SubElement(settings,tag('trackRevisions'))
            parts['word/settings.xml']=etree.tostring(settings,xml_declaration=True,encoding='UTF-8',standalone=True)
            parts['word/comments.xml']=etree.tostring(self.comments,xml_declaration=True,encoding='UTF-8',standalone=True)
            rels=etree.fromstring(parts['word/_rels/document.xml.rels']);ns='http://schemas.openxmlformats.org/package/2006/relationships'
            etree.SubElement(rels,'{'+ns+'}Relationship',Id='rIdRevisionComments',Type=R+'/comments',Target='comments.xml')
            parts['word/_rels/document.xml.rels']=etree.tostring(rels,xml_declaration=True,encoding='UTF-8',standalone=True)
            types=etree.fromstring(parts['[Content_Types].xml']);ns='http://schemas.openxmlformats.org/package/2006/content-types'
            etree.SubElement(types,'{'+ns+'}Override',PartName='/word/comments.xml',ContentType='application/vnd.openxmlformats-officedocument.wordprocessingml.comments+xml')
            parts['[Content_Types].xml']=etree.tostring(types,xml_declaration=True,encoding='UTF-8',standalone=True)
        # Anonymous core metadata; original reference and drawing parts remain intact.
        if 'docProps/core.xml' in parts:
            core=etree.fromstring(parts['docProps/core.xml'])
            for node in core:
                if etree.QName(node).localname in ['creator','lastModifiedBy']:node.text=''
            parts['docProps/core.xml']=etree.tostring(core,xml_declaration=True,encoding='UTF-8',standalone=True)
        parts['word/document.xml']=etree.tostring(root,xml_declaration=True,encoding='UTF-8',standalone=True)
        with ZipFile(path,'w',ZIP_DEFLATED) as z:
            for name,value in parts.items():z.writestr(name,value)
