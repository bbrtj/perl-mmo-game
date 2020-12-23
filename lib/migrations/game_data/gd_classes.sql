INSERT INTO gd_classes (id, playable, base_health, health_per_level, base_health_regen, health_regen_per_level, base_focus, focus_per_level, base_focus_regen, focus_regen_per_level, base_stats, stats_per_level) VALUES
('CLS_WAR', true, 110, 2, 1, 0.2, 100, 3, 4, 0.2, 'STT_INT:10;STT_STA:10;STT_AGI:10;STT_STR:10;STT_CHA:10', 'STT_INT:1;STT_STA:1;STT_AGI:1;STT_STR:1;STT_CHA:1')
,('CLS_ROG', true, 110, 2, 1, 0.2, 100, 3, 4, 0.2, 'STT_INT:10;STT_STA:10;STT_AGI:10;STT_STR:10;STT_CHA:10', 'STT_INT:1;STT_STA:1;STT_AGI:1;STT_STR:1;STT_CHA:1')
,('CLS_BAR', true, 110, 2, 1, 0.2, 100, 3, 4, 0.2, 'STT_INT:10;STT_STA:10;STT_AGI:10;STT_STR:10;STT_CHA:10', 'STT_INT:1;STT_STA:1;STT_AGI:1;STT_STR:1;STT_CHA:1')
,('CLS_PRI', true, 110, 2, 1, 0.2, 100, 3, 4, 0.2, 'STT_INT:10;STT_STA:10;STT_AGI:10;STT_STR:10;STT_CHA:10', 'STT_INT:1;STT_STA:1;STT_AGI:1;STT_STR:1;STT_CHA:1')
,('CLS_PAT', true, 110, 2, 1, 0.2, 100, 3, 4, 0.2, 'STT_INT:10;STT_STA:10;STT_AGI:10;STT_STR:10;STT_CHA:10', 'STT_INT:1;STT_STA:1;STT_AGI:1;STT_STR:1;STT_CHA:1')
,('CLS_SAG', true, 110, 2, 1, 0.2, 100, 3, 4, 0.2, 'STT_INT:10;STT_STA:10;STT_AGI:10;STT_STR:10;STT_CHA:10', 'STT_INT:1;STT_STA:1;STT_AGI:1;STT_STR:1;STT_CHA:1')
,('CLS_ZEA', true, 110, 2, 1, 0.2, 100, 3, 4, 0.2, 'STT_INT:10;STT_STA:10;STT_AGI:10;STT_STR:10;STT_CHA:10', 'STT_INT:1;STT_STA:1;STT_AGI:1;STT_STR:1;STT_CHA:1')
,('CLS_WIT', true, 110, 2, 1, 0.2, 100, 3, 4, 0.2, 'STT_INT:10;STT_STA:10;STT_AGI:10;STT_STR:10;STT_CHA:10', 'STT_INT:1;STT_STA:1;STT_AGI:1;STT_STR:1;STT_CHA:1')
;

INSERT INTO gd_class_abilities (class_id, ability_id) VALUES
('CLS_SAG', 'ABI_STRIKE')
,('CLS_WAR', 'ABI_STRIKE')
,('CLS_ROG', 'ABI_STRIKE')
,('CLS_BAR', 'ABI_STRIKE')
,('CLS_PRI', 'ABI_STRIKE')
,('CLS_PAT', 'ABI_STRIKE')
,('CLS_ZEA', 'ABI_STRIKE')
,('CLS_WIT', 'ABI_STRIKE')
;
