
import I from 'Block.Bridge'

// Scheme

<scheme>
  <type data={type}/>

  <script>
    this.type = opts.data.value1
  </script>
</scheme>

// Type

<type>
  <type-id  if={cons == 'id'} data={type}/>
  <type-var if={cons == 'var'} data={type}/>

  <script>
    this.type = opts.data.value0
    this.cons = I.tcons(this.type)
  </script>
</type>

<type-id class='term type'>
  <span class='token'>{opts.data.value0}</span>
</type-id>

<type-var class='term type'>
  <span class='token'>{opts.data.value0}</span>
</type-var>
