#section FRAGMENT_SHADER
#include (trial::trial "post/color-adjustment.glsl")
uniform sampler2D previous_pass;
uniform sampler2D depth_map;
uniform sampler2D ui_map;
out vec4 color;
in vec2 uv;

void main(){
  color.a = 1.0;
  color.rgb = post_process(previous_pass, uv);
  
  vec4 ui = texture(ui_map, uv);
  color.rgb = mix(color.rgb, ui.rgb, ui.a);
}
