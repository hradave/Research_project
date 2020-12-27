heating = readxl::read_xlsx('data\\heating.xlsx')

colnames = c("Relative_Compactness", "Surface_Area", "Wall_Area", "Roof_Area", "Overall_Height", 
             "Orientation", "Glazing_Area", "Glazing_Area_Distribution", "Heating_Load", "Cooling_Load")
colnames(heating) = colnames

plot(heating$Surface_Area, heating$Cooling_Load, pch = 20)
