// based on http://stackoverflow.com/a/32340906/1974918
// and http://stackoverflow.com/a/8774101/1974918
$(document).keydown(function(event) {
  if ($("#reg_run").is(":visible") && (event.metaKey || event.ctrlKey) && event.keyCode == 13) {
    $("#reg_run").click();
  } else if ($("#logit_run").is(":visible") && (event.metaKey || event.ctrlKey) && event.keyCode == 13) {
    $("#logit_run").click();
  } else if ($("#nb_run").is(":visible") && (event.metaKey || event.ctrlKey) && event.keyCode == 13) {
    $("#nb_run").click();
  } else if ($("#ann_run").is(":visible") && (event.metaKey || event.ctrlKey) && event.keyCode == 13) {
    $("#ann_run").click();
  } else if ($("#crs_run").is(":visible") && (event.metaKey || event.ctrlKey) && event.keyCode == 13) {
    $("#crs_run").click();
  } else if ($("#crtree_run").is(":visible") && (event.metaKey || event.ctrlKey) && event.keyCode == 13) {
    $("#crtree_run").click();
  } else if ($("#dtree_eval").is(":visible") && (event.metaKey || event.ctrlKey) && event.keyCode == 13) {
    $("#dtree_eval").click();
  } else if ($("#dtree_eval_plot").is(":visible") && (event.metaKey || event.ctrlKey) && event.keyCode == 13) {
    $("#dtree_eval_plot").click();
  } else if ($("#dtree_eval_sense").is(":visible") && (event.metaKey || event.ctrlKey) && event.keyCode == 13) {
    $("#dtree_eval_sense").click();
  } else if ($("#dtree_eval_sensitivity").is(":visible") && (event.metaKey || event.ctrlKey) && event.keyCode == 13) {
    $("#dtree_eval_sensitivity").click();
  } else if ($("#ebin_run").is(":visible") && (event.metaKey || event.ctrlKey) && event.keyCode == 13) {
    $("#ebin_run").click();
  } else if ($("#ereg_run").is(":visible") && (event.metaKey || event.ctrlKey) && event.keyCode == 13) {
    $("#ereg_run").click();
  } else if ($("#runSim").is(":visible") && (event.metaKey || event.ctrlKey) && event.keyCode == 13) {
    $("#runSim").click();
  } else if ($("#runRepeat").is(":visible") && (event.metaKey || event.ctrlKey) && event.keyCode == 13) {
    $("#runRepeat").click();
  }
});
