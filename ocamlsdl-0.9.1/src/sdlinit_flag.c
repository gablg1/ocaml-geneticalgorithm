/* init_flag : conversion table */
lookup_info ml_table_init_flag[] = {
  { 0, 8 },
  { MLTAG_CDROM, SDL_INIT_CDROM },
  { MLTAG_JOYSTICK, SDL_INIT_JOYSTICK },
  { MLTAG_EVENTTHREAD, SDL_INIT_EVENTTHREAD },
  { MLTAG_TIMER, SDL_INIT_TIMER },
  { MLTAG_EVERYTHING, SDL_INIT_EVERYTHING },
  { MLTAG_NOPARACHUTE, SDL_INIT_NOPARACHUTE },
  { MLTAG_AUDIO, SDL_INIT_AUDIO },
  { MLTAG_VIDEO, SDL_INIT_VIDEO },
};
