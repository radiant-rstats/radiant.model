$(document).keydown(function(event) {
  // focusing in text (area) inputs

  if (event.metaKey === false && event.ctrlKey === false && event.shiftKey === false) {

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
    } else if ($("#sim_binom_p").is(":focus") && event.keyCode == 13) {
      $("#sim_binom_add").click();
    } else if ($("#sim_discrete_prob").is(":focus") && event.keyCode == 13) {
      $("#sim_discrete_add").click();
    } else if ($("#sim_lnorm_sd").is(":focus") && event.keyCode == 13) {
      $("#sim_lnorm_add").click();
    } else if ($("#sim_norm_sd").is(":focus") && event.keyCode == 13) {
      $("#sim_norm_add").click();
    } else if ($("#sim_pois_lambda").is(":focus") && event.keyCode == 13) {
      $("#sim_pois_add").click();
    } else if ($("#sim_unif_max").is(":focus") && event.keyCode == 13) {
      $("#sim_unif_add").click();
    } else if ($("#sim_const_nr").is(":focus") && event.keyCode == 13) {
      $("#sim_const_add").click();
    } else if ($("#sim_grid_step").is(":focus") && event.keyCode == 13) {
      $("#sim_grid_add").click();
    } else if ($("#sim_sequ_max").is(":focus") && event.keyCode == 13) {
      $("#sim_sequ_add").click();
    } else if ($("#rep_grid_step").is(":focus") && event.keyCode == 13) {
      $("#rep_grid_add").click();
    } else if ($("#dtree_sense_step").is(":focus") && event.keyCode == 13) {
      $("#dtree_sense_add").click();
    }
  }

  if ($("#dtree_load_yaml").is(":visible") && (event.metaKey || event.ctrlKey) &&
      event.shiftKey === false && event.keyCode == 79) {
    $("#dtree_load_yaml").click();
    event.preventDefault();
  } else if ($("#dtree_save_yaml").is(":visible") && (event.metaKey || event.ctrlKey) &&
     event.shiftKey === false && event.keyCode == 83) {
    $("#dtree_save_yaml").click();
    event.preventDefault();
  }
});
