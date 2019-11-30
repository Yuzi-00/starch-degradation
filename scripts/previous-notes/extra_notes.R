sub("m", "xxxxx", df$watertable)
# look for a particular parttern and replace it 
# looking for "m", and replace it with a value
# sub only replace the first match
# $: watertable within the df dataframe

gsub(...same as before)
# gsub: general sub --> with replace all the "m", not just the first one

df %>% 
  mutate(watertable_new = sub("m", "xxxxx", df$watertable))

df %>% 
  mutate(watertable_new = sub(".$", "xxxxx", df$watertable))
# .$: any chr that is before the end of the string

df %>% 
  mutate(watertable_new = sub("\\.", "xxxxx", df$watertable))
# \\: not treat it as a symbol

df %>% 
  mutate(watertable_new = sub(".+", "xxxxx", df$watertable))
# find any chr and keep finding all of thing like that and replace the whole...

df %>% 
  mutate(watertable_new = sub("[abcd]", "xxxxx", df$watertable))
# anything within this grp

df %>% 
  mutate(watertable_new = sub("[0-9]*", "xxxxx", df$watertable))
# * : griddy match

df %>% 
  mutate(watertable_new = sub("[^0-9]*", "xxxxx", df$watertable))
# ^: anything is not sth, here: not 0-9

df %>% 
  mutate(watertable_new = gsub("[a-z0-9]+", "", df$watertable))

#important
df %>% 
  mutate(watertable_new = sub("[^0-9]*([0-9.]*).*", "\\1", df$watertable)) %>% 
  view # quick view of the data

# useful
df %>% 
  mutate(watertable_number = sub("[^0-9]*([0-9\\.]*)(.*)", "\\1", df$watertable),
         # extract the number
         watertable_text = sub("[^0-9]*([0-9\\.]*)(.*)", "\\2", df$watertable))
# extract the string

