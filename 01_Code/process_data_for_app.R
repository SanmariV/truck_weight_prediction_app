fix_wheels_function <-
function(data){
  
  return_this <- data %>% 
    separate(RearWheels, into = c("RearWheels_category_1","RearWheels_category_2","RearWheels_category_3"), sep = ", ", remove = FALSE) %>% 
    separate(RearWheels_category_1, into = c("RearWheels_category_1_1","RearWheels_category_1_2"), sep = "x") %>% 
    mutate(rearwheels_aluminum = RearWheels %>% str_to_lower() %>% str_detect("aluminum") %>% as.numeric()) %>% 
    mutate(rearwheels_steel = RearWheels %>% str_to_lower() %>% str_detect("steel") %>% as.numeric()) %>% 
    mutate(RearWheels_category_1_1 = as.numeric(RearWheels_category_1_1)) %>% 
    mutate(RearWheels_category_1_2 = as.numeric(RearWheels_category_1_2)) %>%
    mutate(RearWheels_category_3 = as.numeric(RearWheels_category_3)) %>%
    separate(FrontWheels, into = c("FrontWheels_category_1","FrontWheels_category_2","FrontWheels_category_3"), sep = ", ", remove = FALSE) %>% 
    separate(FrontWheels_category_1, into = c("FrontWheels_category_1_1","FrontWheels_category_1_2"), sep = "x") %>% 
    mutate(frontwheels_aluminum = FrontWheels %>% str_to_lower() %>% str_detect("aluminum") %>% as.numeric()) %>% 
    mutate(frontwheels_steel = FrontWheels %>% str_to_lower() %>% str_detect("steel") %>% as.numeric()) %>% 
    mutate(FrontWheels_category_1_1 = as.numeric(FrontWheels_category_1_1)) %>% 
    mutate(FrontWheels_category_1_2 = as.numeric(FrontWheels_category_1_2)) %>%
    mutate(FrontWheels_category_3 = as.numeric(FrontWheels_category_3)) %>%
    mutate(Overhang = as.integer(Overhang)) %>%
    mutate(WheelBase = as.integer(WheelBase)) %>%
    select(-RearWheels_category_2, -FrontWheels_category_2) 
  
  return(return_this)
  
}
convert_string_to_factor <-
function(data){
    
    string_cols <- sapply(data, is.character)
    
    # Convert identified string columns to factors
    
    data[string_cols] <- lapply(data[string_cols], as.factor)
    
    return(data)
    
  }
predict_function <-
function(new_data, 
                             model  = model_final
                            ) {
  new_data %>%
    predict(model, new_data = .)
}
feature_importance_plot_function <-
function(model=NULL, target=NULL){
    
    target_expr <- quo_name(target)
    
    feature_importance_plot <- model %>% 
      vip(
        num_features = 20,
        geom = "point",
        aesthetics= list(
          size = 2,
          colour = "#18bc9c"
        )
      ) +
      theme_light(base_size = 10) +
      
      labs(title = "Top 20 Features")
    
    return(feature_importance_plot)
    
  }
generate_truck_weight <-
function(
    Engine=NULL,
    Transmission=NULL,
    FrontAxlePosition=NULL,
    WheelBase=NULL,
    Overhang=NULL,
    FrameRails=NULL,
    Liner=NULL,
    FrontEndExt=NULL,
    Cab=NULL,
    RearAxels=NULL,
    RearSusp=NULL,
    FrontSusp=NULL,
    RearTires=NULL,
    FrontTires=NULL,
    TagAxle=NULL,
    EngineFamily=NULL,
    TransmissionFamily=NULL,
    RearWheels=NULL,
    FrontWheels=NULL,
    .ml_model=NULL){
  
    
    new_truck_prediction <- tibble(
      Engine = Engine,
      Transmission = Transmission,
      FrontAxlePosition = FrontAxlePosition,
      WheelBase = WheelBase,
      Overhang = Overhang,
      FrameRails = FrameRails,
      Liner = Liner,
      FrontEndExt = FrontEndExt,
      Cab = Cab,
      RearAxels = RearAxels,
      RearSusp = RearSusp,
      FrontSusp = FrontSusp,
      RearTires = RearTires,
      FrontTires = FrontTires,
      TagAxle = TagAxle,
      EngineFamily = EngineFamily,
      TransmissionFamily = TransmissionFamily,
      RearWheels = RearWheels,
      FrontWheels = FrontWheels
    )
    
    pred <- new_truck_prediction %>% 
      fix_wheels_function() %>% 
      convert_string_to_factor() %>%  
      predict(.ml_model, new_data = .)
    
    return(pred)
    
  }
