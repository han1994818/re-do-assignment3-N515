# Load the tidyverse package, which includes multiple subpackages for data processing and visualization加载tidyverse包，该包包括了用于数据处理和可视化的多个子包
library(tidyverse)

# Set the path to the data file设置数据文件的路径
file_path <- "/Users/lichenghan/Desktop/Mcdaniel college/515/redo 515assignments/assignment-3/StormEvents_details-ftp_v1.0_d1950_c20210803.csv"

# Reading CSV Files读取CSV文件
storm_data <- read_csv(file_path)

# Filter necessary columns and process them筛选必要的列并进行处理
storm_data <- storm_data %>%
  select(BEGIN_YEARMONTH, EPISODE_ID, STATE, STATE_FIPS, CZ_NAME, CZ_TYPE, CZ_FIPS, EVENT_TYPE) %>%
  mutate(
    STATE = str_to_title(STATE),
    CZ_NAME = str_to_title(CZ_NAME)
  ) %>%
  filter(CZ_TYPE == "C") %>%
  select(-CZ_TYPE) %>%
  mutate(
    STATE_FIPS = str_pad(STATE_FIPS, width = 2, pad = "0"),
    CZ_FIPS = str_pad(CZ_FIPS, width = 3, pad = "0")
  ) %>%
  unite("new_fips", STATE_FIPS, CZ_FIPS, sep = "")

# Change all column names to lowercase更改所有列名为小写
storm_data <- storm_data %>%
  rename_with(tolower)

# Load the built-in state information data and create a related data frame 加载内置的州信息数据并创建相关数据框
data("state")
state_info <- data.frame(state_name = state.name, area = state.area, region = state.region)

# Count incidents by state and consolidate state information 按州统计事件数并合并州信息
events_per_state <- storm_data %>%
  group_by(state) %>%
  summarise(number_of_events = n()) %>%
  left_join(state_info, by = c("state" = "state_name")) %>%
  filter(!is.na(area))

# Creating graphs with ggplot2 使用ggplot2创建图形
ggplot(events_per_state, aes(x = area, y = number_of_events, color = region)) +
  geom_point() +
  labs(title = "Number of Storm Events by State Area", x = "Land Area (Square Miles)", y = "Number of Storm Events") +
  scale_color_brewer(palette = "Set1")

