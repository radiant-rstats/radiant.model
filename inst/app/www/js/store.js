$(document).keydown(function(event) {
  // focusing in text (area) inputs
  if ($("#reg_store_res_name").is(":focus") && event.keyCode == 13) {
    $("#reg_store_res").click();
  } else if ($("#reg_store_pred_name").is(":focus") && event.keyCode == 13) {
    $("#reg_store_pred").click();
  }else if ($("#logit_store_res_name").is(":focus") && event.keyCode == 13) {
    $("#logit_store_res").click();
  } else if ($("#logit_store_pred_name").is(":focus") && event.keyCode == 13) {
    $("#logit_store_pred").click();
  } else if ($("#nb_store_pred_name").is(":focus") && event.keyCode == 13) {
    $("#nb_store_pred").click();
  } else if ($("#nn_store_res_name").is(":focus") && event.keyCode == 13) {
    $("#nn_store_res").click();
  } else if ($("#nn_store_pred_name").is(":focus") && event.keyCode == 13) {
    $("#nn_store_pred").click();
  } else if ($("#crtree_store_res_name").is(":focus") && event.keyCode == 13) {
    $("#crtree_store_res").click();
  } else if ($("#crtree_store_pred_name").is(":focus") && event.keyCode == 13) {
    $("#crtree_store_pred").click();
  } else if ($("#crs_store_pred_name").is(":focus") && event.keyCode == 13) {
    $("#crs_store_pred").click();
  }
});
