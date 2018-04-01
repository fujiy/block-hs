// Info

import Mixin from './mixin.js'
import I from 'Block.Bridge'


<type-info hidden={!opts.show}>
  <scheme data={opts.data}/>
</type-info>

<infos>
  <error each={d in data.errors} data={d}/>
  <script>
    this.mixin(Mixin.Data)
  </script>
</infos>


<error>
  <div if={cons == 'var'}>
    <span class='token'>Variable not in scope:</span>
    <div class='term var'><span class='token'>{data.value0}</span></div>
  </div>
  <div if={cons == 'match'}>
    <span class='token'>Couldn't match expected type</span>
    <type data={data.value0} outer={true}/>
    <span class='token'>with</span>
    <type data={data.value1} outer={true}/>
  </div>
  <div if={cons == 'occurs'}>
    <span class='token'>Occurs check: cannot construct the infinite type:</span>
    <type data={data.value0} outer={true}/>
    <span class='token'>~</span>
    <type data={data.value1} outer={true}/>
  </div>
  <script>
    this.mixin(Mixin.Data)
    this.cons = I.errcons(this.data)
  </script>
</error>
