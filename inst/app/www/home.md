<div class = "col-sm-12", style="width:100vw;height:100vh;">
<style>
summary:focus { 
outline: none 
}
</style>
<h2>Find New NLR among Your sequences and RefPlantNLR</h2>

<p style="text-align:justify; text-justify:inter-ideograph;font-size:20px;">This application allows you to download, blast and align your sequences against RefPlantNLR then construct the phylogenetic tree. 

<HR>

<p style="text-align:justify; text-justify:inter-ideograph;font-size:20px;"><a href ="https://journals.plos.org/plosbiology/article?id=10.1371/journal.pbio.3001124/">RefPlantNLR</a> is a comprehensive collection of experimentally validated plant disease resistance proteins from the NLR family. RefPlantNLR consists of 481 NLRs from 31 genera belonging to 11 orders of flowering plants. This reference dataset has several applications. We used RefPlantNLR to determine the canonical features of functionally validated plant NLRs and to benchmark 5 NLR annotation tools. This revealed that although NLR annotation tools tend to retrieve the majority of NLRs, they frequently produce domain architectures that are inconsistent with the RefPlantNLR annotation. Guided by this analysis, we developed a new pipeline, NLRtracker, which extracts and annotates NLRs from protein or transcript files based on the core features found in the RefPlantNLR dataset. The RefPlantNLR dataset should also prove useful for guiding comparative analyses of NLRs across the wide spectrum of plant diversity and identifying understudied taxa. We hope that the RefPlantNLR resource will contribute to moving the field beyond a uniform view of NLR structure and function.</p>

<p  style="text-align:justify; text-justify:inter-ideograph;font-size:20px">
<code>shiny4RefPlantNLR</code>, an interactive Shiny/R web-based application that enables the user to use RefPlantNLR dataset conveniently. Shiny4RefPlantNLR enables a user to (i)download the RefPlantNLR fasta-type data and the corresponding in-formation table, full or partial by filtering, (ii)blast their fasta-type data to the RefPlantNLR and download the corresponding result, (iii)align multiple sequences under the algorithm Clustal Omega(Sievers and Higgins, 2014; Sievers, et al., 2011) and then plot the results of alignment using ggplot2(Wickham, 2011), (iv)construct the phyloge-netic tree using FastTree v2.1.11(Price, et al., 2010) or RAxML v8.2.12 (Stamatakis, 2014) and then visualize the phylogenetic tree using ggtree v3.1.6(Yu, et al., 2017).
</p>

<p style="text-align:justify; text-justify:inter-ideograph;font-size:20px;">You can also jump to:</p>

<ul>
<li><a href = "javascript:void(0)" onclick = "download()">Download</a></li>
<li> <a href = "javascript:void(0)" onclick = "blast()">Blast</a></li>
<li> <a href = "javascript:void(0)" onclick = "alignment()">Alignment</a></li>
<li> <a href = "javascript:void(0)" onclick = "tree()">Phylogenetic Tree</a></li>
<li> <a href = "javascript:void(0)" onclick = "about()">About</a></li>
</ul>

<script>
function download() {
  $("a[data-value ='Download'").click()
}
function blast() {
  $("a[data-value ='Blast'").click()
}
function alignment() {
  $("a[data-value ='Alignment'").click()
}
function tree() {
  $("a[data-value ='Phylogenetic Tree'").click()
}
function about() {
  $("a[data-value ='About'").click()
}
</script>

</div>

