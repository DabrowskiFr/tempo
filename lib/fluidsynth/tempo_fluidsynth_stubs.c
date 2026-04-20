#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/unixsupport.h>

#include <fluidsynth.h>

#include <stdlib.h>
#include <string.h>

typedef struct {
  fluid_settings_t *settings;
  fluid_synth_t *synth;
  fluid_audio_driver_t *driver;
  int sfont_id;
} tempo_fluidsynth_t;

typedef struct {
  int tick;
  int kind;
  int channel;
  int a;
  int b;
} imported_event_t;

typedef struct {
  imported_event_t *events;
  size_t len;
  size_t cap;
  int current_tick;
  int division;
  int tempo_us_per_quarter;
} import_context_t;

static void finalize_tempo_fluidsynth(value v)
{
  tempo_fluidsynth_t *handle = *(tempo_fluidsynth_t **)Data_custom_val(v);
  if (handle == NULL) {
    return;
  }
  if (handle->driver != NULL) {
    delete_fluid_audio_driver(handle->driver);
  }
  if (handle->synth != NULL) {
    delete_fluid_synth(handle->synth);
  }
  if (handle->settings != NULL) {
    delete_fluid_settings(handle->settings);
  }
  free(handle);
  *(tempo_fluidsynth_t **)Data_custom_val(v) = NULL;
}

static struct custom_operations tempo_fluidsynth_ops = {
  "tempo.fluidsynth",
  finalize_tempo_fluidsynth,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

static tempo_fluidsynth_t *tempo_fluidsynth_handle(value v)
{
  tempo_fluidsynth_t *handle = *(tempo_fluidsynth_t **)Data_custom_val(v);
  if (handle == NULL) {
    caml_failwith("tempo_fluidsynth: closed handle");
  }
  return handle;
}

static void check_fluid(int status, const char *message)
{
  if (status == FLUID_FAILED) {
    caml_failwith(message);
  }
}

static void append_imported_event(import_context_t *ctx, int kind, int channel, int a, int b)
{
  if (ctx->len == ctx->cap) {
    size_t next_cap = ctx->cap == 0 ? 64 : ctx->cap * 2;
    imported_event_t *next =
        (imported_event_t *)realloc(ctx->events, next_cap * sizeof(imported_event_t));
    if (next == NULL) {
      caml_raise_out_of_memory();
    }
    ctx->events = next;
    ctx->cap = next_cap;
  }
  ctx->events[ctx->len].tick = ctx->current_tick;
  ctx->events[ctx->len].kind = kind;
  ctx->events[ctx->len].channel = channel;
  ctx->events[ctx->len].a = a;
  ctx->events[ctx->len].b = b;
  ctx->len += 1;
}

static int on_midi_tick(void *data, int tick)
{
  import_context_t *ctx = (import_context_t *)data;
  ctx->current_tick = tick;
  return FLUID_OK;
}

static int on_midi_event(void *data, fluid_midi_event_t *event)
{
  import_context_t *ctx = (import_context_t *)data;
  int type = fluid_midi_event_get_type(event);
  int channel = fluid_midi_event_get_channel(event);
  switch (type) {
  case 0x80:
    append_imported_event(
        ctx, 1, channel, fluid_midi_event_get_key(event), 0);
    break;
  case 0x90:
    append_imported_event(
        ctx,
        0,
        channel,
        fluid_midi_event_get_key(event),
        fluid_midi_event_get_velocity(event));
    break;
  case 0xB0:
    append_imported_event(
        ctx,
        2,
        channel,
        fluid_midi_event_get_control(event),
        fluid_midi_event_get_value(event));
    break;
  case 0xC0:
    append_imported_event(
        ctx,
        3,
        channel,
        fluid_midi_event_get_program(event),
        0);
    break;
  default:
    break;
  }
  return FLUID_OK;
}

static value imported_event_to_value(imported_event_t *event)
{
  CAMLparam0();
  CAMLlocal3(payload, pair, result);

  switch (event->kind) {
  case 0:
    payload = caml_alloc(3, 0);
    Store_field(payload, 0, Val_int(event->channel));
    Store_field(payload, 1, Val_int(event->a));
    Store_field(payload, 2, Val_int(event->b));
    break;
  case 1:
    payload = caml_alloc(2, 1);
    Store_field(payload, 0, Val_int(event->channel));
    Store_field(payload, 1, Val_int(event->a));
    break;
  case 2:
    payload = caml_alloc(3, 2);
    Store_field(payload, 0, Val_int(event->channel));
    Store_field(payload, 1, Val_int(event->a));
    Store_field(payload, 2, Val_int(event->b));
    break;
  case 3:
    payload = caml_alloc(2, 3);
    Store_field(payload, 0, Val_int(event->channel));
    Store_field(payload, 1, Val_int(event->a));
    break;
  default:
    payload = Val_unit;
    break;
  }

  pair = caml_alloc(2, 0);
  Store_field(pair, 0, Val_int(event->tick));
  Store_field(pair, 1, payload);
  result = pair;
  CAMLreturn(result);
}

CAMLprim value caml_tempo_fluidsynth_create(value path_v, value gain_v)
{
  CAMLparam2(path_v, gain_v);
  CAMLlocal1(result);

  tempo_fluidsynth_t *handle = malloc(sizeof(*handle));
  if (handle == NULL) {
    caml_raise_out_of_memory();
  }
  memset(handle, 0, sizeof(*handle));

  handle->settings = new_fluid_settings();
  if (handle->settings == NULL) {
    free(handle);
    caml_failwith("new_fluid_settings failed");
  }
  fluid_settings_setnum(handle->settings, "synth.gain", Double_val(gain_v));
  handle->synth = new_fluid_synth(handle->settings);
  if (handle->synth == NULL) {
    delete_fluid_settings(handle->settings);
    free(handle);
    caml_failwith("new_fluid_synth failed");
  }
  handle->sfont_id = fluid_synth_sfload(handle->synth, String_val(path_v), 1);
  if (handle->sfont_id == FLUID_FAILED) {
    delete_fluid_synth(handle->synth);
    delete_fluid_settings(handle->settings);
    free(handle);
    caml_failwith("fluid_synth_sfload failed");
  }
  handle->driver = new_fluid_audio_driver(handle->settings, handle->synth);
  if (handle->driver == NULL) {
    delete_fluid_synth(handle->synth);
    delete_fluid_settings(handle->settings);
    free(handle);
    caml_failwith("new_fluid_audio_driver failed");
  }

  result = caml_alloc_custom(&tempo_fluidsynth_ops, sizeof(tempo_fluidsynth_t *), 0, 1);
  *(tempo_fluidsynth_t **)Data_custom_val(result) = handle;
  CAMLreturn(result);
}

CAMLprim value caml_tempo_fluidsynth_shutdown(value synth_v)
{
  CAMLparam1(synth_v);
  finalize_tempo_fluidsynth(synth_v);
  CAMLreturn(Val_unit);
}

CAMLprim value caml_tempo_fluidsynth_program_select(
    value synth_v, value channel_v, value bank_v, value preset_v)
{
  CAMLparam4(synth_v, channel_v, bank_v, preset_v);
  tempo_fluidsynth_t *handle = tempo_fluidsynth_handle(synth_v);
  check_fluid(
      fluid_synth_program_select(
          handle->synth,
          Int_val(channel_v),
          handle->sfont_id,
          Int_val(bank_v),
          Int_val(preset_v)),
      "fluid_synth_program_select failed");
  CAMLreturn(Val_unit);
}

CAMLprim value caml_tempo_fluidsynth_note_on(
    value synth_v, value channel_v, value key_v, value velocity_v)
{
  CAMLparam4(synth_v, channel_v, key_v, velocity_v);
  tempo_fluidsynth_t *handle = tempo_fluidsynth_handle(synth_v);
  check_fluid(
      fluid_synth_noteon(
          handle->synth, Int_val(channel_v), Int_val(key_v), Int_val(velocity_v)),
      "fluid_synth_noteon failed");
  CAMLreturn(Val_unit);
}

CAMLprim value caml_tempo_fluidsynth_note_off(
    value synth_v, value channel_v, value key_v)
{
  CAMLparam3(synth_v, channel_v, key_v);
  tempo_fluidsynth_t *handle = tempo_fluidsynth_handle(synth_v);
  /* FluidSynth may report failure when a note is already stopped.
     For the Tempo showcase, releasing an already-ended note should be benign. */
  (void)fluid_synth_noteoff(handle->synth, Int_val(channel_v), Int_val(key_v));
  CAMLreturn(Val_unit);
}

CAMLprim value caml_tempo_fluidsynth_all_notes_off(value synth_v, value channel_v)
{
  CAMLparam2(synth_v, channel_v);
  tempo_fluidsynth_t *handle = tempo_fluidsynth_handle(synth_v);
  (void)fluid_synth_all_notes_off(handle->synth, Int_val(channel_v));
  CAMLreturn(Val_unit);
}

CAMLprim value caml_tempo_fluidsynth_control_change(
    value synth_v, value channel_v, value control_v, value value_v)
{
  CAMLparam4(synth_v, channel_v, control_v, value_v);
  tempo_fluidsynth_t *handle = tempo_fluidsynth_handle(synth_v);
  check_fluid(
      fluid_synth_cc(
          handle->synth,
          Int_val(channel_v),
          Int_val(control_v),
          Int_val(value_v)),
      "fluid_synth_cc failed");
  CAMLreturn(Val_unit);
}

CAMLprim value caml_tempo_fluidsynth_import_midi_file(value path_v)
{
  CAMLparam1(path_v);
  CAMLlocal4(result, list, cons, pair);

  fluid_settings_t *settings = new_fluid_settings();
  fluid_synth_t *synth = NULL;
  fluid_player_t *player = NULL;
  import_context_t ctx;

  memset(&ctx, 0, sizeof(ctx));
  if (settings == NULL) {
    caml_failwith("new_fluid_settings failed");
  }
  synth = new_fluid_synth(settings);
  if (synth == NULL) {
    delete_fluid_settings(settings);
    caml_failwith("new_fluid_synth failed");
  }
  player = new_fluid_player(synth);
  if (player == NULL) {
    delete_fluid_synth(synth);
    delete_fluid_settings(settings);
    caml_failwith("new_fluid_player failed");
  }
  if (fluid_player_add(player, String_val(path_v)) != FLUID_OK) {
    delete_fluid_player(player);
    delete_fluid_synth(synth);
    delete_fluid_settings(settings);
    caml_failwith("fluid_player_add failed");
  }

  ctx.division = fluid_player_get_division(player);
  ctx.tempo_us_per_quarter = fluid_player_get_midi_tempo(player);

  fluid_player_set_tick_callback(player, on_midi_tick, &ctx);
  fluid_player_set_playback_callback(player, on_midi_event, &ctx);
  if (fluid_player_set_tempo(player, FLUID_PLAYER_TEMPO_EXTERNAL_BPM, 60000.0)
      != FLUID_OK) {
    delete_fluid_player(player);
    delete_fluid_synth(synth);
    delete_fluid_settings(settings);
    free(ctx.events);
    caml_failwith("fluid_player_set_tempo failed");
  }
  if (fluid_player_play(player) != FLUID_OK) {
    delete_fluid_player(player);
    delete_fluid_synth(synth);
    delete_fluid_settings(settings);
    free(ctx.events);
    caml_failwith("fluid_player_play failed");
  }
  fluid_player_join(player);

  list = Val_emptylist;
  for (ssize_t i = (ssize_t)ctx.len - 1; i >= 0; --i) {
    pair = imported_event_to_value(&ctx.events[i]);
    cons = caml_alloc(2, 0);
    Store_field(cons, 0, pair);
    Store_field(cons, 1, list);
    list = cons;
  }

  result = caml_alloc(3, 0);
  Store_field(result, 0, Val_int(ctx.division));
  Store_field(result, 1, Val_int(ctx.tempo_us_per_quarter));
  Store_field(result, 2, list);

  free(ctx.events);
  delete_fluid_player(player);
  delete_fluid_synth(synth);
  delete_fluid_settings(settings);
  CAMLreturn(result);
}
