ggplot(merged_ml, (aes(x=ncrural, y=RUCC_2013)))+
  geom_jitter(alpha=0.6)+
  scale_y_continuous(breaks=pretty_breaks())

rucc_map<-ggplot()+
  geom_sf(data=merged_ml, aes(fill=RUCC_2013), color=NA)+
  geom_sf(data=mainland_wgs, fill=NA)+
  scale_fill_continuous(breaks=pretty_breaks())+
  theme_void()

metro_map<-ggplot()+
  geom_sf(data=merged_ml, aes(fill=metro_class), color=NA)+
  geom_sf(data=mainland_wgs, fill=NA)+
  scale_fill_continuous(breaks=pretty_breaks(n=2))+
  theme_void()

pdf("ruccvmetro.pdf", width=8, height=11)
ggarrange(rucc_map, metro_map,
          ncol=1, nrow=2, common.legend = F)
dev.off()

metro_va<-ggplot()+
  geom_sf(data=merged_va, aes(fill=metro_class), color=NA)+
  #geom_sf(data=mainland_wgs, fill=NA)+
  scale_fill_continuous(breaks=pretty_breaks(n=2))+
  theme_void()

